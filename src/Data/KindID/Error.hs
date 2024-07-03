{-# LANGUAGE UndecidableInstances #-}

-- | An internal module to provide better compilation errors for invalid
-- 'Data.KindID.V7.KindID' prefixes.
--
module Data.KindID.Error where

import           Data.Kind
import           Data.TypeID.Error
import           Data.Type.Bool
import           Data.Type.Equality
import           Data.Type.Ord
import           GHC.TypeLits

-- | A class that covnerts a poly-kind into an "ErrorMessage".
--
-- Used for converting type-level "TypeIDError"s into custom compile-time error
-- messages.
class ToErrorMessageC (e :: k) where
  type ToErrorMessage e :: ErrorMessage

instance ToErrorMessageC (TypeIDErrorPrefixTooLong t) where
  type ToErrorMessage (TypeIDErrorPrefixTooLong t) =
        Text "The prefix "
   :<>: ShowType (Str2Sym t)
   :<>: Text " with "
   :<>: ShowType (LengthSymbol (Str2Sym t))
   :<>: Text " characters is too long!"

instance ToErrorMessageC TypeIDExtraSeparator where
  type ToErrorMessage TypeIDExtraSeparator = Text
    "The underscore separator should not be present if the prefix is empty!"

instance ToErrorMessageC (TypeIDStartWithUnderscore t) where
  type ToErrorMessage (TypeIDStartWithUnderscore t) =
        Text "The prefix "
   :<>: ShowType (Str2Sym t)
   :<>: Text " should not start with an underscore!"

instance ToErrorMessageC (TypeIDEndWithUnderscore t) where
  type ToErrorMessage (TypeIDEndWithUnderscore t) =
        Text "The prefix "
   :<>: ShowType (Str2Sym t)
   :<>: Text " should not end with an underscore!"

instance ToErrorMessageC (TypeIDErrorPrefixInvalidChar t c) where
  type ToErrorMessage (TypeIDErrorPrefixInvalidChar t c) =
        Text "The prefix "
   :<>: ShowType (Str2Sym t)
   :<>: Text " contains invalid character "
   :<>: ShowType c
   :<>: Text "!"

type family Str2Sym (str :: String) :: Symbol where
  Str2Sym '[]       = ""
  Str2Sym (c ': cs) = ConsSymbol c (Str2Sym cs)

type family Sym2Str (str :: Symbol) :: String where
  Sym2Str s = SSUH (UnconsSymbol s)

type family SSUH (uncons :: Maybe (Char, Symbol)) :: String where
  SSUH Nothing          = '[]
  SSUH (Just '( c, s )) = c ': Sym2Str s

-- | The length of a 'Symbol' as a 'Nat'.
type family LengthSymbol (prefix :: Symbol) :: Nat where
  LengthSymbol prefix = LSUH (UnconsSymbol prefix)

-- | LengthSymbol Uncons Helper.
type family LSUH (uncons :: Maybe (Char, Symbol)) :: Nat where
  LSUH 'Nothing          = 0
  LSUH ('Just '( c, s )) = 1 + LengthSymbol s

-- | Is a type-level 'Char' lowercase?
type family IsLowerChar (ch :: Char) :: Bool where
  IsLowerChar ch = Compare '`' ch == 'LT && Compare ch '{' == 'LT

-- | Is a type-level 'Char' an underscore?
type family IsUnderscore (ch :: Char) :: Bool where
  IsUnderscore ch = Compare '_' ch == 'EQ

-- | Is a 'Symbol' lowercase + underscore and not start or end with underscores?
--
-- ''Nothing' if no error; otherwise ''Just' a 'TypeIDError'.
type family IsLUSymbol (prefix :: Symbol) :: Maybe TypeIDError where
  IsLUSymbol "_"    = 'Just 'TypeIDExtraSeparator
  IsLUSymbol prefix = ILUSUH1 (UnconsSymbol prefix) prefix

-- | First IsLUSymbol Uncons Helper.
type family ILUSUH1 (uncons :: Maybe (Char, Symbol)) (prefix :: Symbol)
  :: Maybe TypeIDError where
    ILUSUH1 'Nothing s            = 'Nothing
    ILUSUH1 ('Just '( '_', _ )) s = 'Just
      (TypeIDStartWithUnderscore (Sym2Str s))
    ILUSUH1 ('Just '( c, r )) s   =
          WrapMaybe (IsLowerChar c || IsUnderscore c)
                    (TypeIDErrorPrefixInvalidChar (Sym2Str s) c)
      <|> ILUSUH2 (UnconsSymbol r) s

-- | Second IsLUSymbol Uncons Helper.
type family ILUSUH2 (uncons :: Maybe (Char, Symbol)) (prefix :: Symbol)
  :: Maybe TypeIDError where
    ILUSUH2 'Nothing s             = 'Nothing
    ILUSUH2 ('Just '( '_', "" )) s = 'Just (TypeIDEndWithUnderscore (Sym2Str s))
    ILUSUH2 ('Just '( c, r )) s    =
          WrapMaybe (IsLowerChar c || IsUnderscore c)
                    (TypeIDErrorPrefixInvalidChar (Sym2Str s) c)
      <|> ILUSUH2 (UnconsSymbol r) s

-- | @wrapMaybe True _ = Nothing; wrapMaybe False x = Just x@.
type family WrapMaybe p a where
  WrapMaybe True a  = 'Nothing
  WrapMaybe False a = 'Just a

-- | Type-level '(<|>)' for 'Maybe'.
type family (<|>) a b where
  'Just a <|> b = 'Just a
  Nothing <|> b = b

-- | Turn a type-level 'Maybe' 'TypeIDError' into a 'TypeError'.
type family BuildTypeIDErrorConstraint (a :: Maybe TypeIDError)
  :: Constraint where
    BuildTypeIDErrorConstraint 'Nothing  = ()
    BuildTypeIDErrorConstraint ('Just e) = TypeError (ToErrorMessage e)
