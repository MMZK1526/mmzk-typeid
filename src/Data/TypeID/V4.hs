-- |
-- Module      : Data.TypeID.V4
-- License     : MIT
-- Maintainer  : mmzk1526@outlook.com
-- Portability : GHC
--
-- 'Data.TypeID.V7.TypeID' with 'UUID'v4.
--
module Data.TypeID.V4
  (
  -- * Data types
    TypeIDV4
  , getPrefix
  , getUUID
  -- * 'TypeIDV4' generation ('TypeIDV4'-specific)
  , genTypeID
  , genTypeID'
  , decorateTypeID
  -- * 'TypeIDV4' generation (class methods)
  , genID
  , genID'
  , genIDs
  , decorate
  -- * Validation ('TypeIDV4'-specific)
  , checkPrefix
  , checkTypeID
  -- * Validation (class methods)
  , checkID
  -- * Encoding & decoding ('TypeIDV4'-specific)
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
import           Data.Word

-- | Similar to 'Data.TypeID.V7.TypeID', but uses 'UUID'v4.
type TypeIDV4 = TID.TypeID' 'V4

-- | Generate a new 'TypeIDV4' from a prefix.
--
-- It throws a 'TypeIDError' if the prefix does not match the specification,
-- namely if it's longer than 63 characters or if it contains characters other
-- than lowercase latin letters.
genTypeID :: MonadIO m => Text -> m TypeIDV4
genTypeID = TID.genTypeIDV4
{-# INLINE genTypeID #-}

-- | Generate a new 'TypeIDV4' from a prefix based on insecure 'UUID'v4.
genTypeID' :: MonadIO m => Text -> m TypeIDV4
genTypeID' = TID.genTypeIDV4'
{-# INLINE genTypeID' #-}

-- | Obtain a 'TypeIDV4' from a prefix and a 'UUID'.
decorateTypeID :: Text -> UUID -> Either TypeIDError TypeIDV4
decorateTypeID = TID.decorateTypeID
{-# INLINE decorateTypeID #-}

-- | Check if the given prefix is a valid 'TypeIDV4' prefix.
checkPrefix :: Text -> Maybe TypeIDError
checkPrefix = TID.checkPrefix
{-# INLINE checkPrefix #-}

-- | Check if the prefix is valid and the suffix 'UUID' has the correct v4
-- version and variant.
checkTypeID :: TypeIDV4 -> Maybe TypeIDError
checkTypeID = TID.checkTypeIDV4
{-# INLINE checkTypeID #-}

-- | Pretty-print a 'TypeIDV4'. It is 'id2String' with concrete type.
toString :: TypeIDV4 -> String
toString = TID.toString
{-# INLINE toString #-}

-- | Pretty-print a 'TypeIDV4' to strict 'Text'. It is 'id2Text' with concrete
-- type.
toText :: TypeIDV4 -> Text
toText = TID.toText
{-# INLINE toText #-}

-- | Pretty-print a 'TypeIDV4' to lazy 'ByteString'. It is 'id2ByteString'
-- with concrete type.
toByteString :: TypeIDV4 -> ByteString
toByteString = TID.toByteString
{-# INLINE toByteString #-}

-- | Parse a 'TypeIDV4' from its 'String' representation. It is 'string2ID' with
-- concrete type.
parseString :: String -> Either TypeIDError TypeIDV4
parseString = TID.parseString
{-# INLINE parseString #-}

-- | Parse a 'TypeIDV4' from its string representation as a strict 'Text'. It
-- is 'text2ID' with concrete type.
parseText :: Text -> Either TypeIDError TypeIDV4
parseText = TID.parseText
{-# INLINE parseText #-}

-- | Parse a 'TypeIDV4' from its string representation as a lazy 'ByteString'.
-- It is 'byteString2ID' with concrete type.
parseByteString :: ByteString -> Either TypeIDError TypeIDV4
parseByteString = TID.parseByteString
{-# INLINE parseByteString #-}

-- | Parse a 'TypeIDV4' from its 'String' representation, throwing an error when
-- the parsing fails. It is 'string2IDM' with concrete type.
parseStringM :: MonadIO m => String -> m TypeIDV4
parseStringM = TID.parseStringM
{-# INLINE parseStringM #-}

-- | Parse a 'TypeIDV4' from its string representation as a strict 'Text',
-- throwing an error when the parsing fails. It is 'text2IDM' with concrete
-- type.
parseTextM :: MonadIO m => Text -> m TypeIDV4
parseTextM = TID.parseTextM
{-# INLINE parseTextM #-}

-- | Parse a 'TypeIDV4' from its string representation as a lazy 'ByteString',
-- throwing an error when the parsing fails. It is 'byteString2IDM' with
-- concrete type.
parseByteStringM :: MonadIO m => ByteString -> m TypeIDV4
parseByteStringM = TID.parseByteStringM
{-# INLINE parseByteStringM #-}
