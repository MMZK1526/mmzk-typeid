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
newtype KindID prefix = KindID { _getUUID :: UUID }
  deriving (Eq, Ord, Show)

-- | A constraint for valid prefix 'Symbol's.
type ValidPrefix prefix = ( KnownSymbol prefix
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

-- | A class that translates any kind to a 'Symbol'. It is used to translate
-- custom data kinds to a 'Symbol' so that they can be used as 'KindID'
-- prefixes.
--
-- For example, suppose we have the following data structure that represents the
-- prefixes we are going to use:
--
-- > data Prefix = User | Post | Comment
--
-- Then we can make it an instance of 'ToPrefix' like this:
--
-- > instance ToPrefix 'User where
-- >   type PrefixSymbol 'User = "user"
-- >
-- > instance ToPrefix 'Post where
-- >   type PrefixSymbol 'Post = "post"
-- >
-- > instance ToPrefix 'Comment where
-- >   type PrefixSymbol 'Comment = "comment"
--
-- Now we can use 'Prefix' as a prefix for 'KindID's, e.g.
--
-- > do
-- >   userID <- genKindID @'User -- Same as genKindID @"user"
-- >   postID <- genKindID @'Post -- Same as genKindID @"post"
-- >   commentID <- genKindID @'Comment -- Same as genKindID @"comment"
class ToPrefix a where
  type PrefixSymbol a :: Symbol

instance ToPrefix (a :: Symbol) where
  type PrefixSymbol a = a
