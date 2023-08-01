module Data.TypeID.V7
  (
  -- * Data types
    TypeID
  , TypeIDV7
  , getPrefix
  , getUUID
  , getTime
  -- * 'TypeID' generation ('TypeID'-specific)
  , genTypeID
  , genTypeID'
  , genTypeIDs
  , decorateTypeID
  -- * 'TypeID' generation (class methods)
  , genID
  , genID'
  , genIDs
  , decorate
  -- * Validation ('TypeID'-specific)
  , checkPrefix
  , checkTypeID
  , checkTypeIDWithEnv
  -- * Validation (class methods)
  , checkID
  , checkIDWithEnv
  -- * Encoding & decoding ('TypeID'-specific)
  , toString
  , toText
  , toByteString
  , parseString
  , parseText
  , parseByteString
  , parseStringM
  , parseTextM
  , parseByteStringM
  -- * Encoding & decoding (class methods)
  , id2String
  , id2Text
  , id2ByteString
  , string2ID
  , text2ID
  , byteString2ID
  , string2IDM
  , text2IDM
  , byteString2IDM
  ) where

import           Control.Monad.IO.Class
import           Data.ByteString.Lazy (ByteString)
import           Data.Text (Text)
import           Data.TypeID.Class
import           Data.TypeID.Error
import           Data.TypeID.Internal (TypeID)
import qualified Data.TypeID.Internal as TID
import           Data.UUID.Types (UUID)
import           Data.Word

-- | A type alias for 'TypeID'.
type TypeIDV7 = TypeID

-- | Generate a new 'TypeID' from a prefix.
--
-- It throws a 'TypeIDError' if the prefix does not match the specification,
-- namely if it's longer than 63 characters or if it contains characters other
-- than lowercase latin letters.
genTypeID :: MonadIO m => Text -> m TypeID
genTypeID = TID.genTypeID
{-# INLINE genTypeID #-}

-- | Generate a new 'TypeID' from a prefix based on statelesss 'UUID'v7.
--
-- See the documentation of 'V7.genUUID'' for more information.
genTypeID' :: MonadIO m => Text -> m TypeID
genTypeID' = TID.genTypeID'
{-# INLINE genTypeID' #-}

-- | Generate a list of 'TypeID's from a prefix.
--
-- It tries its best to generate 'TypeID's at the same timestamp, but it may not
-- be possible if we are asking too many 'UUID's at the same time.
--
-- It is guaranteed that the first 32768 'TypeID's are generated at the same
-- timestamp.
genTypeIDs :: MonadIO m => Text -> Word16 -> m [TypeID]
genTypeIDs = TID.genTypeIDs
{-# INLINE genTypeIDs #-}

-- | Obtain a 'TypeID' from a prefix and a 'UUID'.
decorateTypeID :: Text -> UUID -> Either TypeIDError TypeID
decorateTypeID = TID.decorateTypeID
{-# INLINE decorateTypeID #-}

-- | Check if the given prefix is a valid TypeID prefix.
checkPrefix :: Text -> Maybe TypeIDError
checkPrefix = TID.checkPrefix
{-# INLINE checkPrefix #-}

-- | Check if the prefix is valid and the suffix 'UUID' has the correct v7
-- version and variant.
checkTypeID :: TypeID -> Maybe TypeIDError
checkTypeID = TID.checkTypeID
{-# INLINE checkTypeID #-}

-- | Similar to 'checkTypeID', but also checks if the suffix 'UUID' is
-- generated in the past.
checkTypeIDWithEnv :: MonadIO m => TypeID -> m (Maybe TypeIDError)
checkTypeIDWithEnv = TID.checkTypeIDWithEnv
{-# INLINE checkTypeIDWithEnv #-}

-- | Pretty-print a 'TypeID'. It is 'id2String' with concrete type.
toString :: TypeID -> String
toString = TID.toString
{-# INLINE toString #-}

-- | Pretty-print a 'TypeID' to strict 'Text'. It is 'id2Text' with concrete
-- type.
toText :: TypeID -> Text
toText = TID.toText
{-# INLINE toText #-}

-- | Pretty-print a 'TypeID' to strict 'ByteString'. It is 'id2ByteString' with
-- concrete type.
toByteString :: TypeID -> ByteString
toByteString = TID.toByteString
{-# INLINE toByteString #-}

-- | Parse a 'TypeID' from its 'String' representation. It is 'string2ID' with
-- concrete type.
parseString :: String -> Either TypeIDError TypeID
parseString = TID.parseString
{-# INLINE parseString #-}

-- | Parse a 'TypeID' from its strict 'Text' representation. It is 'text2ID'
-- with concrete type.
parseText :: Text -> Either TypeIDError TypeID
parseText = TID.parseText
{-# INLINE parseText #-}

-- | Parse a 'TypeID' from its strict 'ByteString' representation. It is
-- 'byteString2ID' with concrete type.
parseByteString :: ByteString -> Either TypeIDError TypeID
parseByteString = TID.parseByteString
{-# INLINE parseByteString #-}

-- | Parse a 'TypeID' from its 'String' representation, throwing an error when
-- the parsing fails. It is 'string2IDM' with concrete type.
parseStringM :: MonadIO m => String -> m TypeID
parseStringM = TID.parseStringM
{-# INLINE parseStringM #-}

-- | Parse a 'TypeID' from its strict 'Text' representation, throwing an error
-- when the parsing fails. It is 'text2IDM' with concrete type.
parseTextM :: MonadIO m => Text -> m TypeID
parseTextM = TID.parseTextM
{-# INLINE parseTextM #-}

-- | Parse a 'TypeID' from its strict 'ByteString' representation, throwing an
-- error when the parsing fails. It is 'byteString2IDM' with concrete type.
parseByteStringM :: MonadIO m => ByteString -> m TypeID
parseByteStringM = TID.parseByteStringM
{-# INLINE parseByteStringM #-}
