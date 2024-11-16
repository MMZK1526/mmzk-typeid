{-# LANGUAGE CPP #-}
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
  ) where

#ifndef __HADDOCK_VERSION__
import           Data.Kind
import           Data.KindID.Error
import           Data.Type.Bool
import           Data.Type.Equality
import           Data.Type.Ord
import           Data.TypeID.Error
#endif
import           GHC.TypeLits

-- | A class that translates any kind to a 'Symbol'. It is used to translate
-- custom data kinds to a 'Symbol' so that they can be used as
-- 'Data.KindID.KindID' prefixes.
--
-- For example, suppose we have the following data structure that represents the
-- prefixes we are going to use:
--
-- > data Prefix = User | Post | Comment | SuperUser
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
-- >
-- > instance ToPrefix 'SuperUser where
-- >   type PrefixSymbol 'SuperUser = "super_user"
--
-- Now we can use Prefix as a prefix for 'Data.KindID.KindID's, e.g.
--
-- > do
-- >   userID    <- genKindID @'User      -- Same as genKindID @"user"
-- >   postID    <- genKindID @'Post      -- Same as genKindID @"post"
-- >   commentID <- genKindID @'Comment   -- Same as genKindID @"comment"
-- >   suID      <- genKindID @'SuperUser -- Same as genKindID @"super_user"
class ToPrefix a where
  -- | The associated type family that converts @a@ into a 'Symbol'.
  type PrefixSymbol a :: Symbol

-- | The 'PrefixSymbol' of a 'Symbol' is the 'Symbol' itself.
instance ToPrefix (s :: Symbol) where
  type PrefixSymbol s = s

#ifndef __HADDOCK_VERSION__
-- | A constraint for valid prefix 'Symbol's.
--
type ValidPrefix prefix = ( KnownSymbol prefix
                          , LengthLT64C prefix
                          , IsLUSymbolC prefix )
#else
-- | A constraint for valid prefix 'Symbol's.
--
-- Note that this is __NOT__ the actual definition! Its true definition is
-- hidden here in the documentation as it uses internal type-level helpers that
-- we do not expose and make no guarantee on their stability. In practice, any
-- prefix with this constraint is a valid prefix for a 'Data.KindID.KindID'.
type ValidPrefix prefix = KnownSymbol prefix
#endif

#ifndef __HADDOCK_VERSION__
-- | Contains a custom error message if the prefix 'Symbol' is too long.
type family LengthLT64C (prefix :: Symbol) :: Constraint where
  LengthLT64C s
    = If (Compare (LengthSymbol s) 64 == 'LT) (() :: Constraint)
         (TypeError (ToErrorMessage ('TypeIDErrorPrefixTooLong (Sym2Str s))))

-- | Contains a custom error message if the prefix 'Symbol' is not lowercase +
-- underscore or it starts or ends with underscores.
type family IsLUSymbolC (prefix :: Symbol) :: Constraint where
  IsLUSymbolC s = BuildTypeIDErrorConstraint (IsLUSymbol s)
#endif
