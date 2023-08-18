-- |
-- Module      : Data.TypeID.V1
-- License     : MIT
-- Maintainer  : mmzk1526@outlook.com
-- Portability : GHC
--
-- 'Data.TypeID.V7.TypeID' with 'UUID'v1.
--
module Data.TypeID.V1
  (
  -- * Data types
    TypeIDV1
  , getPrefix
  , getUUID
  -- * 'TypeIDV1' generation ('TypeIDV1'-specific)
  , genTypeID
  , decorateTypeID
  -- * 'TypeIDV1' generation (class methods)
  , genID
  , decorate
  -- * Validation ('TypeIDV1'-specific)
  , checkPrefix
  , checkTypeID
  -- * Validation (class methods)
  , checkID
  -- * Encoding & decoding ('TypeIDV1'-specific)
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
import qualified Data.TypeID.Internal as TID
import           Data.UUID.Types (UUID)
import           Data.UUID.Versions

-- | Similar to 'Data.TypeID.V7.TypeID', but uses 'UUID'v1.
type TypeIDV1 = TID.TypeID' 'V1

-- | Generate a new 'TypeIDV1' from a prefix.
--
-- It throws a 'TypeIDError' if the prefix does not match the specification,
-- namely if it's longer than 63 characters or if it contains characters other
-- than lowercase latin letters.
genTypeID :: MonadIO m => Text -> m TypeIDV1
genTypeID = TID.genTypeIDV1
{-# INLINE genTypeID #-}

-- | Obtain a 'TypeIDV1' from a prefix and a 'UUID'.
decorateTypeID :: Text -> UUID -> Either TypeIDError TypeIDV1
decorateTypeID = TID.decorateTypeID
{-# INLINE decorateTypeID #-}

-- | Check if the given prefix is a valid 'TypeIDV1' prefix.
checkPrefix :: Text -> Maybe TypeIDError
checkPrefix = TID.checkPrefix
{-# INLINE checkPrefix #-}

-- | Check if the prefix is valid and the suffix 'UUID' has the correct v1
-- version and variant.
checkTypeID :: TypeIDV1 -> Maybe TypeIDError
checkTypeID = TID.checkTypeIDV1
{-# INLINE checkTypeID #-}

-- | Pretty-print a 'TypeIDV1'. It is 'id2String' with concrete type.
toString :: TypeIDV1 -> String
toString = TID.toString
{-# INLINE toString #-}

-- | Pretty-print a 'TypeIDV1' to strict 'Text'. It is 'id2Text' with concrete
-- type.
toText :: TypeIDV1 -> Text
toText = TID.toText
{-# INLINE toText #-}

-- | Pretty-print a 'TypeIDV1' to lazy 'ByteString'. It is 'id2ByteString'
-- with concrete type.
toByteString :: TypeIDV1 -> ByteString
toByteString = TID.toByteString
{-# INLINE toByteString #-}

-- | Parse a 'TypeIDV1' from its 'String' representation. It is 'string2ID' with
-- concrete type.
parseString :: String -> Either TypeIDError TypeIDV1
parseString = TID.parseString
{-# INLINE parseString #-}

-- | Parse a 'TypeIDV1' from its string representation as a strict 'Text'. It
-- is 'text2ID' with concrete type.
parseText :: Text -> Either TypeIDError TypeIDV1
parseText = TID.parseText
{-# INLINE parseText #-}

-- | Parse a 'TypeIDV1' from its string representation as a lazy 'ByteString'.
-- It is 'byteString2ID' with concrete type.
parseByteString :: ByteString -> Either TypeIDError TypeIDV1
parseByteString = TID.parseByteString
{-# INLINE parseByteString #-}

-- | Parse a 'TypeIDV1' from its 'String' representation, throwing an error when
-- the parsing fails. It is 'string2IDM' with concrete type.
parseStringM :: MonadIO m => String -> m TypeIDV1
parseStringM = TID.parseStringM
{-# INLINE parseStringM #-}

-- | Parse a 'TypeIDV1' from its string representation as a strict 'Text',
-- throwing an error when the parsing fails. It is 'text2IDM' with concrete
-- type.
parseTextM :: MonadIO m => Text -> m TypeIDV1
parseTextM = TID.parseTextM
{-# INLINE parseTextM #-}

-- | Parse a 'TypeIDV1' from its string representation as a lazy 'ByteString',
-- throwing an error when the parsing fails. It is 'byteString2IDM' with
-- concrete type.
parseByteStringM :: MonadIO m => ByteString -> m TypeIDV1
parseByteStringM = TID.parseByteStringM
{-# INLINE parseByteStringM #-}
