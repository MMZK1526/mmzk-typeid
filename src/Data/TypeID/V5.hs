-- |
-- Module      : Data.TypeID.V5
-- License     : MIT
-- Maintainer  : mmzk1526@outlook.com
-- Portability : GHC
--
-- 'Data.TypeID.V7.TypeID' with 'UUID'v5.
--
module Data.TypeID.V5
  (
  -- * Data types
    TypeIDV5
  , getPrefix
  , getUUID
  -- * 'TypeIDV5' generation ('TypeIDV5'-specific)
  , genTypeID
  , decorateTypeID
  -- * 'TypeIDV5' generation (class methods)
  , genID
  , decorate
  -- * Validation ('TypeIDV5'-specific)
  , checkPrefix
  , checkTypeID
  -- * Validation (class methods)
  , checkID
  -- * Encoding & decoding ('TypeIDV5'-specific)
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

-- | Similar to 'Data.TypeID.V7.TypeID', but uses 'UUID'v5.
type TypeIDV5 = TID.TypeID' 'V5

-- | Generate a new 'TypeIDV5' from a prefix.
--
-- It throws a 'TypeIDError' if the prefix does not match the specification,
-- namely if it's longer than 63 characters or if it contains characters other
-- than lowercase latin letters.
genTypeID :: MonadIO m => Text -> UUID -> [Word8] -> m TypeIDV5
genTypeID = TID.genTypeIDV5
{-# INLINE genTypeID #-}

-- | Obtain a 'TypeIDV5' from a prefix and a 'UUID'.
decorateTypeID :: Text -> UUID -> Either TypeIDError TypeIDV5
decorateTypeID = TID.decorateTypeID
{-# INLINE decorateTypeID #-}

-- | Check if the given prefix is a valid 'TypeIDV5' prefix.
checkPrefix :: Text -> Maybe TypeIDError
checkPrefix = TID.checkPrefix
{-# INLINE checkPrefix #-}

-- | Check if the prefix is valid and the suffix 'UUID' has the correct v5
-- version and variant.
checkTypeID :: TypeIDV5 -> Maybe TypeIDError
checkTypeID = TID.checkTypeIDV5
{-# INLINE checkTypeID #-}

-- | Pretty-print a 'TypeIDV5'. It is 'id2String' with concrete type.
toString :: TypeIDV5 -> String
toString = TID.toString
{-# INLINE toString #-}

-- | Pretty-print a 'TypeIDV5' to strict 'Text'. It is 'id2Text' with concrete
-- type.
toText :: TypeIDV5 -> Text
toText = TID.toText
{-# INLINE toText #-}

-- | Pretty-print a 'TypeIDV5' to lazy 'ByteString'. It is 'id2ByteString'
-- with concrete type.
toByteString :: TypeIDV5 -> ByteString
toByteString = TID.toByteString
{-# INLINE toByteString #-}

-- | Parse a 'TypeIDV5' from its 'String' representation. It is 'string2ID' with
-- concrete type.
parseString :: String -> Either TypeIDError TypeIDV5
parseString = TID.parseString
{-# INLINE parseString #-}

-- | Parse a 'TypeIDV5' from its string representation as a strict 'Text'. It
-- is 'text2ID' with concrete type.
parseText :: Text -> Either TypeIDError TypeIDV5
parseText = TID.parseText
{-# INLINE parseText #-}

-- | Parse a 'TypeIDV5' from its string representation as a lazy 'ByteString'.
-- It is 'byteString2ID' with concrete type.
parseByteString :: ByteString -> Either TypeIDError TypeIDV5
parseByteString = TID.parseByteString
{-# INLINE parseByteString #-}

-- | Parse a 'TypeIDV5' from its 'String' representation, throwing an error when
-- the parsing fails. It is 'string2IDM' with concrete type.
parseStringM :: MonadIO m => String -> m TypeIDV5
parseStringM = TID.parseStringM
{-# INLINE parseStringM #-}

-- | Parse a 'TypeIDV5' from its string representation as a strict 'Text',
-- throwing an error when the parsing fails. It is 'text2IDM' with concrete
-- type.
parseTextM :: MonadIO m => Text -> m TypeIDV5
parseTextM = TID.parseTextM
{-# INLINE parseTextM #-}

-- | Parse a 'TypeIDV5' from its string representation as a lazy 'ByteString',
-- throwing an error when the parsing fails. It is 'byteString2IDM' with
-- concrete type.
parseByteStringM :: MonadIO m => ByteString -> m TypeIDV5
parseByteStringM = TID.parseByteStringM
{-# INLINE parseByteStringM #-}
