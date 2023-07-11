{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.KindID.Internal where

import           Data.Type.Bool
import           Data.Type.Equality
import           Data.Type.Ord
import           Data.UUID.V7 (UUID)
import           GHC.TypeLits hiding (Text)

-- | A TypeID with the prefix encoded at type level.
--
-- It is dubbed 'KindID' because we the prefix here is a data kind rather than
-- a type.
--
-- Note that the 'Show' instance is for debugging purposes only. To pretty-print
-- a 'KindID', use 'toString', 'toText' or 'toByteString'.
newtype KindID (prefix :: Symbol) = KindID { _getUUID :: UUID }
  deriving (Eq, Ord, Show)

-- | A constraint for valid prefix 'Symbol's.
type ValidPrefix (prefix :: Symbol) = ( KnownSymbol prefix
                                      , LengthSymbol prefix < 64
                                      , IsLowerSymbol prefix ~ 'True )

type family LengthSymbol (prefix :: Symbol) :: Nat where
  LengthSymbol prefix = LSUH (UnconsSymbol prefix)

-- | Length Symbol Uncons Helper.
type family LSUH (uncons :: Maybe (Char, Symbol)) :: Nat where
  LSUH 'Nothing        = 0
  LSUH ('Just '(c, s)) = 1 + LengthSymbol s

type family IsLowerChar (ch :: Char) :: Bool where
  IsLowerChar ch = Compare '`' ch == LT && Compare ch '{' == LT

type family IsLowerSymbol (prefix :: Symbol) :: Bool where
  IsLowerSymbol prefix = ILSUH (UnconsSymbol prefix)

-- | Is Lower Symbol Uncons Helper.
type family ILSUH (uncons :: Maybe (Char, Symbol)) :: Bool where
  ILSUH 'Nothing        = 'True
  ILSUH ('Just '(c, s)) = IsLowerChar c && IsLowerSymbol s
