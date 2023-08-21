{-# LANGUAGE AllowAmbiguousTypes #-}

-- |
-- Module      : Data.TypeID.Class
-- License     : MIT
-- Maintainer  : mmzk1526@outlook.com
-- Portability : GHC
--
-- A module with the APIs for any 'Data.TypeID.V7.TypeID'-ish identifier type.
--
-- These type classes are useful to define custom TypeID-ish identifier types.
-- For example, if one wishes to remove the constraints on prefix, or use a
-- different UUID version for the suffix.
--
module Data.TypeID.Class
  (
  -- * Type classes
    TypeIDLike
  , IDType(..)
  , IDConv(..)
  , IDGen(..)
  , decorate
  , genID
  , genID'
  , genIDs
  , checkID
  , checkIDWithEnv
  -- * Helper types
  , GenFunc(..)
  , ResWithErr(..)
  ) where

import           Control.Exception
import           Control.Monad.IO.Class
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import           Data.Kind (Type)
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding
import           Data.TypeID.Error
import           Data.UUID.V7 (UUID)
import           Data.Word

-- | A constraint synonym for a 'Data.TypeID.V7.TypeID'-ish identifier type that
-- supports ID generation and string conversion.
type TypeIDLike a = (IDType a, IDConv a, IDGen a)

-- | A type class for a 'Data.TypeID.V7.TypeID'-ish identifier type, which has a
-- 'Text' prefix and a 'UUID' suffix.
class IDType a where
  -- | Get the prefix of the identifier.
  getPrefix :: a -> Text

  -- | Get the UUID suffix of the identifier.
  getUUID :: a -> UUID

  -- | Get the timestamp of the identifier. Returns 0 if the identifier is not
  -- timestamp-based.
  getTime :: a -> Word64

-- | A type class for converting between a 'Data.TypeID.V7.TypeID'-ish
-- identifier type and some string representations.
class IDConv a where
  -- | Parse the identifier from its 'String' representation.
  string2ID :: String -> Either TypeIDError a
  string2ID = text2ID . T.pack
  {-# INLINE string2ID #-}

  -- | Parse the identifier from its string representation as a strict 'Text'.
  text2ID :: Text -> Either TypeIDError a
  text2ID = byteString2ID . BSL.fromStrict . encodeUtf8
  {-# INLINE text2ID #-}

  -- | Parse the identifier from its string representation as a lazy
  -- 'ByteString'.
  byteString2ID :: ByteString -> Either TypeIDError a
  byteString2ID = string2ID . T.unpack . decodeUtf8 . BSL.toStrict
  {-# INLINE byteString2ID #-}

  -- | Pretty-print the identifier to a 'String'.
  id2String :: a -> String
  id2String = T.unpack . id2Text
  {-# INLINE id2String #-}

  -- | Pretty-print the identifier to a strict 'Text'.
  id2Text :: a -> Text
  id2Text = decodeUtf8 . BSL.toStrict . id2ByteString
  {-# INLINE id2Text #-}

  -- | Pretty-print the identifier to a lazy 'ByteString'.
  id2ByteString :: a -> ByteString
  id2ByteString = BSL.fromStrict . encodeUtf8 . T.pack . id2String
  {-# INLINE id2ByteString #-}

  -- | Parse the identifier from its 'String' representation, throwing an error
  -- when the parsing fails.
  string2IDM :: MonadIO m => String -> m a
  string2IDM = either (liftIO . throwIO) pure . string2ID
  {-# INLINE string2IDM #-}

  -- | Parse the identifier from its string representation as a strict 'Text',
  -- throwing an error when the parsing fails.
  text2IDM :: MonadIO m => Text -> m a
  text2IDM = either (liftIO . throwIO) pure . text2ID
  {-# INLINE text2IDM #-}

  -- | Parse the identifier from its string representation as a lazy
  -- 'ByteString', throwing an error when the parsing fails.
  byteString2IDM :: MonadIO m => ByteString -> m a
  byteString2IDM = either (liftIO . throwIO) pure . byteString2ID
  {-# INLINE byteString2IDM #-}

  -- | Parse the identifier from its 'String' representation, but crashes when
  -- the parsing fails.
  unsafeString2ID :: String -> a
  unsafeString2ID = either (error . show) id . string2ID
  {-# INLINE unsafeString2ID #-}

  -- | Parse the identifier from its string representation as a strict 'Text',
  -- but crashes when the parsing fails.
  unsafeText2ID :: Text -> a
  unsafeText2ID = either (error . show) id . text2ID
  {-# INLINE unsafeText2ID #-}

  -- | Parse the identifier from its string representation as a lazy
  -- 'ByteString', but crashes when the parsing fails.
  unsafeByteString2ID :: ByteString -> a
  unsafeByteString2ID = either (error . show) id . byteString2ID
  {-# INLINE unsafeByteString2ID #-}
  {-# MINIMAL string2ID, id2String
            | string2ID, id2Text
            | string2ID, id2ByteString
            | text2ID, id2String
            | text2ID, id2Text
            | text2ID, id2ByteString
            | byteString2ID, id2String
            | byteString2ID, id2Text
            | byteString2ID, id2ByteString #-}

-- | Generate a new identifier with the given prefix.
genID :: forall a m. (IDGen a, MonadIO m)
      => GenFunc (IDGenPrefix a) (IDGenReq a (m a))
genID = genID_ @a @m Proxy
{-# INLINE genID #-}

-- | Similar to 'genID', but stateless. It can be a faster implementation than
-- 'genID', but it does not guarantee any stateful property, such as
-- monotonically increasing for 'UUID'v7-based identifiers.
--
-- The default implementation is the same as 'genID'.
genID' :: forall a m. (IDGen a, MonadIO m)
       => GenFunc (IDGenPrefix a) (IDGenReq a (m a))
genID' = genID'_ @a @m Proxy
{-# INLINE genID' #-}

-- | Generate a list of identifiers with the given prefix.
genIDs :: forall a m. (IDGen a, MonadIO m)
       => GenFunc (IDGenPrefix a) (IDGenReq a (Word16 -> m [a]))
genIDs = genIDs_ @a @m Proxy
{-# INLINE genIDs #-}

-- | Generate a new identifier with the given prefix and 'UUID' suffix.
decorate :: forall a. IDGen a
         => GenFunc (IDGenPrefix a) (UUID -> ResWithErr (IDGenPrefix a) a)
decorate = decorate_ @a Proxy
{-# INLINE decorate #-}

-- | Check the validity of the identifier.
checkID :: forall a. IDGen a => a -> Maybe TypeIDError
checkID = checkID_ @a Proxy
{-# INLINE checkID #-}

-- | Check the validity of the identifier, potentially with impure criteria.
checkIDWithEnv :: forall a m. (IDGen a, MonadIO m) => a -> m (Maybe TypeIDError)
checkIDWithEnv = checkIDWithEnv_ @a @m Proxy
{-# INLINE checkIDWithEnv #-}

-- | A type class for generating 'Data.TypeID.V7.TypeID'-ish identifiers.
--
-- The methods in this type class are not directly used since each of them has
-- a dummy 'Proxy' in order to compile. We implement the methods here and use
-- the methods without the underscore suffix instead.
class IDGen a where
  -- | If the identifier has compile-time determined prefix, this type should be
  -- @'Nothing@. Otherwise it should be @'Just prefix@ where @prefix@ is the
  -- type of the prefix (e.g. 'Text').
  type IDGenPrefix a :: Maybe Type

  -- | If the identifier's generation requires additional information (such as
  -- 'UUID' version 5), this type corresponds to how to generate @r@ from the
  -- required information. Otherwise it should be simply @r@.
  type IDGenReq a r :: Type

  -- | Generate an identifier with the given prefix.
  genID_ :: MonadIO m => Proxy a -> GenFunc (IDGenPrefix a) (IDGenReq a (m a))

  -- | Similar to 'genID'_, but stateless. It can be a faster implementation
  -- than 'genID'_, but it does not guarantee any stateful property, such as
  -- monotonically increasing for 'UUID'v7-based identifiers.
  --
  -- The default implementation is the same as 'genID'_.
  genID'_ :: forall m. MonadIO m
          => Proxy a -> GenFunc (IDGenPrefix a) (IDGenReq a (m a))
  genID'_ = genID_ @_ @m
  {-# INLINE genID'_ #-}

  -- | Generate a list of identifiers with the given prefix.
  genIDs_ :: forall m. MonadIO m
          => Proxy a -> GenFunc (IDGenPrefix a) (IDGenReq a (Word16 -> m [a]))

  -- | Generate a new identifier with the given prefix and 'UUID' suffix.
  decorate_ :: Proxy a
            -> GenFunc (IDGenPrefix a) (UUID -> ResWithErr (IDGenPrefix a) a)

  -- | Check the validity of the identifier.
  checkID_ :: Proxy a -> a -> Maybe TypeIDError

  -- | Check the validity of the identifier, potentially with impure criteria.
  checkIDWithEnv_ :: MonadIO m => Proxy a -> a -> m (Maybe TypeIDError)
  checkIDWithEnv_ _ = pure . checkID_ (Proxy @a)
  {-# INLINE checkIDWithEnv_ #-}

-- | A function generator based on the 'IDGenPrefix' type family.
type family GenFunc prefix res where
  GenFunc ('Just prefix) res = prefix -> res
  GenFunc 'Nothing res       = res

-- | A result that may contain an error, based on the 'IDGenPrefix' type family.
--
-- In other words, if the prefix type is already encoded in the type level,
-- we are certain that the prefix is valid, so the result type does not need the
-- @Either TypeIDError@ part.
type family ResWithErr prefix res where
  ResWithErr ('Just prefix) res = Either TypeIDError res
  ResWithErr 'Nothing res       = res
