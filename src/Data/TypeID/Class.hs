{-# LANGUAGE AllowAmbiguousTypes #-}

-- |
-- Module      : Data.TypeID.Class
-- License     : MIT
-- Maintainer  : mmzk1526@outlook.com
-- Portability : GHC
--
-- A module with the APIs for any 'Data.TypeID'-ish identifier type.
--
-- These type classes are useful to define custom TypeID-ish identifier types.
-- For example, if one wishes to remove the constraints on prefix, or use a
-- different UUID version for the suffix.
--
-- It is not completed as many of the functions are still implemented
-- individually in the "Data.TypeID" and "Data.KindID" modules.
--
module Data.TypeID.Class
  (
  -- * Type classes
    IDType(..)
  , IDConv(..)
  , IDGen(..)
  , decorate
  , genID
  , genID'
  , genIDs
  , GenFunc(..)
  , ResWithErr(..)
  ) where

import           Control.Monad.IO.Class
import           Data.ByteString.Lazy (ByteString)
import           Data.Kind (Type)
import           Data.Proxy
import           Data.Text (Text)
import           Data.TypeID.Error
import           Data.UUID.V7 (UUID)
import           Data.Word

-- | A type class for a TypeID-ish identifier type, which has a 'Text' prefix
-- and a 'UUID' suffix.
class IDType a where
  -- | Get the prefix of the identifier.
  getPrefix :: a -> Text

  -- | Get the UUID suffix of the identifier.
  getUUID :: a -> UUID

  -- | Get the timestamp of the identifier.
  getTime :: a -> Word64

-- | A type class for converting between a TypeID-ish identifier type and some
-- string representations.
class IDConv a where
  -- | Parse the identifier from its 'String' representation.
  string2ID :: String -> Either TypeIDError a

  -- | Parse the identifier from its string representation as a strict 'Text'.
  text2ID :: Text -> Either TypeIDError a

  -- | Parse the identifier from its string representation as a lazy
  -- 'ByteString'.
  byteString2ID :: ByteString -> Either TypeIDError a

  -- | Pretty-print the identifier to a 'String'.
  id2String :: a -> String

  -- | Pretty-print the identifier to a strict 'Text'.
  id2Text :: a -> Text

  -- | Pretty-print the identifier to a lazy 'ByteString'.
  id2ByteString :: a -> ByteString

-- | Generate a new identifier with the given prefix.
genID :: forall a m. (IDGen a, MonadIO m) => GenFunc (IDGenPrefix a) (m a)
genID = genID_ @_ @m (Proxy @a)
{-# INLINE genID #-}

-- | Similar to 'genID', but stateless. It can be a faster implementation than
-- 'genID', but it does not guarantee any stateful property, such as
-- monotonically increasing for 'UUID'v7-based identifiers.
--
-- The default implementation is the same as 'genID'.
genID' :: forall a m. (IDGen a, MonadIO m) => GenFunc (IDGenPrefix a) (m a)
genID' = genID_' @_ @m (Proxy @a)
{-# INLINE genID' #-}

-- | Generate a list of identifiers with the given prefix.
genIDs :: forall a m. (IDGen a, MonadIO m)
       => GenFunc (IDGenPrefix a) (Word16 -> m [a])
genIDs = genIDs_ @_ @m (Proxy @a)
{-# INLINE genIDs #-}

-- | Generate a new identifier with the given prefix and 'UUID' suffix.
decorate :: forall a. IDGen a
         => GenFunc (IDGenPrefix a) (UUID -> ResWithErr (IDGenPrefix a) a)
decorate = decorate_ (Proxy @a)
{-# INLINE decorate #-}

-- | A type class for generating TypeID-ish identifiers.
--
-- The methods in this type class are not directly used since each of them has
-- a dummy 'Proxy' in order to compile. We implement the methods here and use
-- the methods without the underscore suffix instead.
class IDGen a where
  -- | If the identifier has compile-time determined prefix, this type should be
  -- @'Nothing@. Otherwise it should be @'Just prefix@ where @prefix@ is the
  -- type of the prefix (e.g. 'Text').
  type IDGenPrefix a :: Maybe Type

  -- | Generate an identifier with the given prefix.
  genID_ :: MonadIO m => Proxy a -> GenFunc (IDGenPrefix a) (m a)

  -- | Similar to 'genID_', but stateless. It can be a faster implementation
  -- than 'genID_', but it does not guarantee any stateful property, such as
  -- monotonically increasing for 'UUID'v7-based identifiers.
  --
  -- The default implementation is the same as 'genID_'.
  genID_' :: forall m. MonadIO m => Proxy a -> GenFunc (IDGenPrefix a) (m a)
  genID_' = genID_ @_ @m
  {-# INLINE genID_' #-}

  -- | Generate a list of identifiers with the given prefix.
  genIDs_ :: MonadIO m => Proxy a -> GenFunc (IDGenPrefix a) (Word16 -> m [a])

  -- | Generate a new identifier with the given prefix and 'UUID' suffix.
  decorate_ :: Proxy a
            -> GenFunc (IDGenPrefix a) (UUID -> ResWithErr (IDGenPrefix a) a)

-- | A function generator based on the 'IDGenPrefix' type family.
type family GenFunc prefix res where
  GenFunc ('Just prefix) res = prefix -> res
  GenFunc 'Nothing res       = res

-- | A result that may contain an error, based on the 'IDGenPrefix' type family.
--
-- In other words, if the prefix type is already encoded in the type level,
-- we are certain that the prefix is valid, so the result type does not need the
-- "Either TypeIDError" part.
type family ResWithErr prefix res where
  ResWithErr ('Just prefix) res = Either TypeIDError res
  ResWithErr 'Nothing res       = res
