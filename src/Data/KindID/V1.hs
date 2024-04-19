-- |
-- Module      : Data.KindID.V1
-- License     : MIT
-- Maintainer  : mmzk1526@outlook.com
-- Portability : GHC
--
-- 'Data.KindID.V7.KindID' with 'UUID'v1.
--
module Data.KindID.V1
  (
  -- * Data types
    KindIDV1
  , getPrefix
  , getUUID
  , getTime
  -- * 'KindIDV1' generation ('KindIDV1'-specific)
  , genKindID
  , decorateKindID
  -- * 'KindIDV1' generation (class methods)
  , genID
  , decorate
  -- * Validation ('KindIDV1'-specific)
  , checkKindID
  -- * Validation (class methods)
  , checkID
  -- * Encoding & decoding ('KindIDV1'-specific)
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
  -- * Type-level & term-level conversion
  , toTypeID
  , fromTypeID
  ) where

import           Control.Monad.IO.Class
import           Data.ByteString.Lazy (ByteString)
import           Data.KindID.Class
import qualified Data.KindID.Internal as KID
import           Data.Text (Text)
import           Data.TypeID.Class
import           Data.TypeID.Error
import           Data.TypeID.V1 (TypeIDV1)
import           Data.UUID.Types.Internal (UUID)
import           Data.UUID.Versions

-- | Similar to 'Data.KindID.V7.KindID', but uses 'UUID'v1.
type KindIDV1 = KID.KindID' 'V1

-- | Generate a new 'KindIDV1' from a prefix.
--
-- It throws a 'TypeIDError' if the prefix does not match the specification,
-- namely if it's longer than 63 characters or if it contains characters other
-- than lowercase latin letters.
genKindID :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix), MonadIO m)
          => m (KindIDV1 prefix)
genKindID = KID.genKindIDV1
{-# INLINE genKindID #-}

-- | Obtain a 'KindIDV1' from a prefix and a 'UUID'.
decorateKindID :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
               => UUID -> KindIDV1 prefix
decorateKindID = KID.decorateKindID
{-# INLINE decorateKindID #-}

-- | Check if the prefix is valid and the suffix 'UUID' has the correct v1
-- version and variant.
checkKindID :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
            => KindIDV1 prefix -> Maybe TypeIDError
checkKindID = KID.checkKindIDV1
{-# INLINE checkKindID #-}

-- | Pretty-print a 'KindIDV1'. It is 'id2String' with concrete type.
toString :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
         => KindIDV1 prefix -> String
toString = KID.toString
{-# INLINE toString #-}

-- | Pretty-print a 'KindIDV1' to strict 'Text'. It is 'id2Text' with concrete
-- type.
toText :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
       => KindIDV1 prefix -> Text
toText = KID.toText
{-# INLINE toText #-}

-- | Pretty-print a 'KindIDV1' to lazy 'ByteString'. It is 'id2ByteString' with
-- concrete type.
toByteString :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
             => KindIDV1 prefix -> ByteString
toByteString = KID.toByteString
{-# INLINE toByteString #-}

-- | Parse a 'KindIDV1' from its 'String' representation. It is 'string2ID' with
-- concrete type.
parseString :: forall prefix
             . (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
            => String -> Either TypeIDError (KindIDV1 prefix)
parseString = KID.parseString
{-# INLINE parseString #-}

-- | Parse a 'KindIDV1' from its string representation as a strict 'Text'. It is
-- 'text2ID' with concrete type.
parseText :: forall prefix
           . (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
          => Text -> Either TypeIDError (KindIDV1 prefix)
parseText = KID.parseText
{-# INLINE parseText #-}

-- | Parse a 'KindIDV1' from its string representation as a lazy 'ByteString'.
-- It is 'byteString2ID' with concrete type.
parseByteString :: forall prefix
                 . (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
                => ByteString -> Either TypeIDError (KindIDV1 prefix)
parseByteString = KID.parseByteString

-- | Parse a 'KindIDV1' from its 'String' representation, throwing an error when
-- the parsing fails. It is 'string2IDM' with concrete type.
parseStringM :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix), MonadIO m)
             => String -> m (KindIDV1 prefix)
parseStringM = KID.parseStringM
{-# INLINE parseStringM #-}

-- | Parse a 'KindIDV1' from its string representation as a strict 'Text',
-- throwing an error when the parsing fails. It is 'text2IDM' with concrete
-- type.
parseTextM :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix), MonadIO m)
           => Text -> m (KindIDV1 prefix)
parseTextM = KID.parseTextM
{-# INLINE parseTextM #-}

-- | Parse a 'KindIDV1' from its string representation as a lazy 'ByteString',
-- throwing an error when the parsing fails. It is 'byteString2IDM' with
-- concrete type.
parseByteStringM :: ( ToPrefix prefix
                    , ValidPrefix (PrefixSymbol prefix)
                    , MonadIO m )
                 => ByteString -> m (KindIDV1 prefix)
parseByteStringM = KID.parseByteStringM
{-# INLINE parseByteStringM #-}

-- | Convert a 'KindIDV1' to a 'Data.TypeID.V4.TypeIDV1'.
toTypeID :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
         => KindIDV1 prefix -> TypeIDV1
toTypeID = KID.toTypeID
{-# INLINE toTypeID #-}

-- | Convert a 'TypeIDV1' to a 'KindIDV1'. Returns 'Nothing' if the prefix does
-- not match.
fromTypeID :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
           => TypeIDV1 -> Maybe (KindIDV1 prefix)
fromTypeID = KID.fromTypeID
{-# INLINE fromTypeID #-}
