-- |
-- Module      : Data.KindID.V4
-- License     : MIT
-- Maintainer  : mmzk1526@outlook.com
-- Portability : GHC
--
-- 'Data.KindID.V7.KindID' with 'UUID'v4.
--
module Data.KindID.V4
  (
  -- * Data types
    KindIDV4
  , getPrefix
  , getUUID
  , getTime
  -- * 'KindIDV4' generation ('KindIDV4'-specific)
  , genKindID
  , genKindID'
  , decorateKindID
  -- * 'KindIDV4' generation (class methods)
  , genID
  , genID'
  , decorate
  -- * Validation ('KindIDV4'-specific)
  , checkKindID
  -- * Validation (class methods)
  , checkID
  -- * Encoding & decoding ('KindIDV4'-specific)
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
import           Data.KindID.Internal (KindID'(..))
import qualified Data.KindID.Internal as KID
import           Data.Text (Text)
import           Data.TypeID.Class
import           Data.TypeID.Error
import           Data.TypeID.V4 (TypeIDV4)
import           Data.TypeID.V7 (TypeID)
import           Data.UUID.Types.Internal (UUID)
import           Data.UUID.Versions
import           Data.Word

-- | Similar to 'Data.KindID.V7.KindID', but uses 'UUID'v4.
type KindIDV4 = KID.KindID' 'V4

-- | Generate a new 'KindIDV4' from a prefix.
--
-- It throws a 'TypeIDError' if the prefix does not match the specification,
-- namely if it's longer than 63 characters or if it contains characters other
-- than lowercase latin letters.
genKindID :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix), MonadIO m)
          => m (KindIDV4 prefix)
genKindID = KID.genKindIDV4
{-# INLINE genKindID #-}

-- | Generate a new 'KindIDV4' from a prefix using insecure 'UUID'v4.
genKindID' :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix), MonadIO m)
           => m (KindIDV4 prefix)
genKindID' = KID.genKindIDV4'
{-# INLINE genKindID' #-}

-- | Obtain a 'KindIDV4' from a prefix and a 'UUID'.
decorateKindID :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
               => UUID -> KindIDV4 prefix
decorateKindID = KID.decorateKindID
{-# INLINE decorateKindID #-}

-- | Check if the prefix is valid and the suffix 'UUID' has the correct v4
-- version and variant.
checkKindID :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
            => KindIDV4 prefix -> Maybe TypeIDError
checkKindID = KID.checkKindIDV4
{-# INLINE checkKindID #-}


-- | Pretty-print a 'KindIDV4'. It is 'id2String' with concrete type.
toString :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
         => KindIDV4 prefix -> String
toString = KID.toString
{-# INLINE toString #-}

-- | Pretty-print a 'KindIDV4' to strict 'Text'. It is 'id2Text' with concrete
-- type.
toText :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
       => KindIDV4 prefix -> Text
toText = KID.toText
{-# INLINE toText #-}

-- | Pretty-print a 'KindIDV4' to lazy 'ByteString'. It is 'id2ByteString' with
-- concrete type.
toByteString :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
             => KindIDV4 prefix -> ByteString
toByteString = KID.toByteString
{-# INLINE toByteString #-}

-- | Parse a 'KindIDV4' from its 'String' representation. It is 'parseString'
-- with concrete type.
parseString :: forall prefix
             . (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
            => String -> Either TypeIDError (KindIDV4 prefix)
parseString = KID.parseString
{-# INLINE parseString #-}

-- | Parse a 'KindIDV4' from its string representation as a strict 'Text'. It is
-- 'parseText' with concrete type.
parseText :: forall prefix
           . (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
          => Text -> Either TypeIDError (KindIDV4 prefix)
parseText = KID.parseText
{-# INLINE parseText #-}

-- | Parse a 'KindIDV4' from its string representation as a lazy 'ByteString'.
-- It is 'parseByteString' with concrete type.
parseByteString :: forall prefix
                 . (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
                => ByteString -> Either TypeIDError (KindIDV4 prefix)
parseByteString = KID.parseByteString

-- | Parse a 'KindIDV4' from its 'String' representation, throwing an error when
-- the parsing fails. It is 'string2IDM' with concrete type.
parseStringM :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix), MonadIO m)
             => String -> m (KindIDV4 prefix)
parseStringM = KID.parseStringM
{-# INLINE parseStringM #-}

-- | Parse a 'KindIDV4' from its string representation as a strict 'Text',
-- throwing an error when the parsing fails. It is 'text2IDM' with concrete
-- type. It is 'parseTextM' with concrete type.
parseTextM :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix), MonadIO m)
           => Text -> m (KindIDV4 prefix)
parseTextM = KID.parseTextM
{-# INLINE parseTextM #-}

-- | Parse a 'KindIDV4' from its string representation as a lazy 'ByteString',
-- throwing an error when the parsing fails. It is 'byteString2IDM' with
-- concrete type. It is 'parseByteStringM' with concrete type.
parseByteStringM :: ( ToPrefix prefix
                    , ValidPrefix (PrefixSymbol prefix)
                    , MonadIO m )
                 => ByteString -> m (KindIDV4 prefix)
parseByteStringM = KID.parseByteStringM
{-# INLINE parseByteStringM #-}

-- | Convert a 'KindIDV4' to a 'Data.TypeID.V4.TypeIDV4'.
toTypeID :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
         => KindIDV4 prefix -> TypeIDV4
toTypeID = KID.toTypeID
{-# INLINE toTypeID #-}

-- | Convert a 'TypeIDV4' to a 'KindIDV4'.
fromTypeID :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
           => TypeIDV4 -> Maybe (KindIDV4 prefix)
fromTypeID = KID.fromTypeID
{-# INLINE fromTypeID #-}
