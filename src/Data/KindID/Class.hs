{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Data.KindID.Class
-- License     : MIT
-- Maintainer  : mmzk1526@outlook.com
-- Portability : GHC
--
-- This module contains the type-level mechanisms that are used to define
-- custom 'Data.KindID.KindID'-ish identifier types.
--
module Data.KindID.Class
  (
  -- * Prefix
    ValidPrefix
  , ToPrefix(..)
  -- * Helpers
  , LengthSymbol
  , IsLowerChar
  , IsUnderscore
  , IsLUSymbol
  , ILUSUH1
  , ILUSUH2
  -- * Deprecated Helpers
  , IsLowerSymbol
  , LSUH
  , ILSUH
  ) where

import           Data.Type.Bool
import           Data.Type.Equality
import           Data.Type.Ord
import           GHC.TypeLits

-- | A class that translates any kind to a 'Symbol'. It is used to translate
-- custom data kinds to a 'Symbol' so that they can be used as
-- 'Data.KindID.KindID' prefixes.
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
-- Now we can use Prefix as a prefix for 'Data.KindID.KindID's, e.g.
--
-- > do
-- >   userID <- genKindID @'User -- Same as genKindID @"user"
-- >   postID <- genKindID @'Post -- Same as genKindID @"post"
-- >   commentID <- genKindID @'Comment -- Same as genKindID @"comment"
class ToPrefix a where
  type PrefixSymbol a :: Symbol

-- | The 'PrefixSymbol' of a 'Symbol' is the 'Symbol' itself.
instance ToPrefix (a :: Symbol) where
  type PrefixSymbol a = a


-- | A constraint for valid prefix 'Symbol's.
type ValidPrefix prefix = ( KnownSymbol prefix
                          , LengthSymbol prefix < 64
                          , IsLUSymbol prefix ~ 'True )

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
type family IsLUSymbol (prefix :: Symbol) :: Bool where
  IsLUSymbol prefix = ILUSUH1 (UnconsSymbol prefix)

-- | First IsLUSymbol Uncons Helper.
type family ILUSUH1 (uncons :: Maybe (Char, Symbol)) :: Bool where
  ILUSUH1 'Nothing            = True
  ILUSUH1 ('Just '( '_', _ )) = False
  ILUSUH1 ('Just '( c, s ))   = (IsLowerChar c || IsUnderscore c)
                             && ILUSUH2 (UnconsSymbol s)

-- | Second IsLUSymbol Uncons Helper.
type family ILUSUH2 (uncons :: Maybe (Char, Symbol)) :: Bool where
  ILUSUH2 'Nothing           = True
  ILUSUH2 ('Just '( c, "" )) = IsLowerChar c
  ILUSUH2 ('Just '( c, s ))  = (IsLowerChar c || IsUnderscore c)
                            && ILUSUH2 (UnconsSymbol s)

--------------------------------------------------------------------------------
-- Deprecated
--------------------------------------------------------------------------------

-- | Is a 'Symbol' lowercase?
type family IsLowerSymbol (prefix :: Symbol) :: Bool where
  IsLowerSymbol prefix = ILSUH (UnconsSymbol prefix)
{-# DEPRECATED IsLowerSymbol "No longer used; will be removed in the next major version" #-}

-- | Is LowerSymbol Uncons Helper.
type family ILSUH (uncons :: Maybe (Char, Symbol)) :: Bool where
  ILSUH 'Nothing          = 'True
  ILSUH ('Just '( c, s )) = IsLowerChar c && IsLowerSymbol s
{-# DEPRECATED ILSUH "No longer used; will be removed in the next major version" #-}
