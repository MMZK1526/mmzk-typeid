-- |
-- Module      : Data.KindID.V7
-- License     : MIT
-- Maintainer  : mmzk1526@outlook.com
-- Portability : GHC
--
-- Similar to "Data.TypeID", but the type is statically determined in the type
-- level.
--
module Data.KindID.V7
  (
  -- * Data types
    KindID
  , KindIDV7
  , getPrefix
  , getUUID
  , getTime
  -- * 'KindID' generation ('KindID'-specific)
  , genKindID
  , genKindID'
  , genKindIDs
  , decorateKindID
  -- * 'KindID' generation (class methods)
  , genID
  , genID'
  , genIDs
  , decorate
  -- * Validation ('KindID'-specific)
  , checkKindID
  , checkKindIDWithEnv
  -- * Validation (class methods)
  , checkID
  , checkIDWithEnv
  -- * Encoding & decoding ('KindID'-specific)
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
import           Data.TypeID.V7 (TypeID)
import           Data.UUID.Types.Internal (UUID)
import           Data.UUID.Versions
import           Data.Word

-- | A type alias for the default 'KindID' implementation with 'UUID'v7.
type KindID = KID.KindID' 'V7

-- | A type alias for 'KindID'.
type KindIDV7 = KindID

-- | Generate a new 'KindID' from a prefix.
--
-- It throws a 'TypeIDError' if the prefix does not match the specification,
-- namely if it's longer than 63 characters or if it contains characters other
-- than lowercase latin letters.
genKindID :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix), MonadIO m)
          => m (KindID prefix)
genKindID = KID.genKindID
{-# INLINE genKindID #-}

-- | Generate a new 'KindID' from a prefix based on statelesss 'UUID'v7.
--
-- See the documentation of 'V7.genUUID'' for more information.
genKindID' :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix), MonadIO m)
           => m (KindID prefix)
genKindID' = KID.genKindID'
{-# INLINE genKindID' #-}

-- | Generate a list of 'KindID's from a prefix.
--
-- It tries its best to generate 'KindID's at the same timestamp, but it may not
-- be possible if we are asking too many 'UUID's at the same time.
--
-- It is guaranteed that the first 32768 'KindID's are generated at the same
-- timestamp.
genKindIDs :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix), MonadIO m)
           => Word16 -> m [KindID prefix]
genKindIDs = KID.genKindIDs
{-# INLINE genKindIDs #-}

-- | Obtain a 'KindID' from a prefix and a 'UUID'.
decorateKindID :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
               => UUID -> KindID prefix
decorateKindID = KID.decorateKindID
{-# INLINE decorateKindID #-}

-- | Check if the prefix is valid and the suffix 'UUID' has the correct v7
-- version and variant.
checkKindID :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
            => KindID prefix -> Maybe TypeIDError
checkKindID = KID.checkKindID
{-# INLINE checkKindID #-}

-- | Similar to 'checkKindID', but also checks if the suffix 'UUID' is
-- generated in the past.
checkKindIDWithEnv :: ( ToPrefix prefix
                      , ValidPrefix (PrefixSymbol prefix)
                      , MonadIO m )
                   => KindID' 'V7 prefix -> m (Maybe TypeIDError)
checkKindIDWithEnv = KID.checkKindIDWithEnv
{-# INLINE checkKindIDWithEnv #-}

-- | Pretty-print a 'KindID'. It is 'id2String' with concrete type.
toString :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
         => KindID prefix -> String
toString = KID.toString
{-# INLINE toString #-}

-- | Pretty-print a 'KindID' to strict 'Text'. It is 'id2Text' with concrete
-- type.
toText :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
       => KindID prefix -> Text
toText = KID.toText
{-# INLINE toText #-}

-- | Pretty-print a 'KindID' to lazy 'ByteString'. It is 'id2ByteString' with
-- concrete type.
toByteString :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
             => KindID prefix -> ByteString
toByteString = KID.toByteString
{-# INLINE toByteString #-}

-- | Parse a 'KindID' from its 'String' representation. It is 'parseString'
-- with concrete type.
parseString :: forall prefix
             . (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
            => String -> Either TypeIDError (KindID prefix)
parseString = KID.parseString
{-# INLINE parseString #-}

-- | Parse a 'KindID' from its string representation as a strict 'Text'. It is
-- 'parseText' with concrete type.
parseText :: forall prefix
           . (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
          => Text -> Either TypeIDError (KindID prefix)
parseText = KID.parseText
{-# INLINE parseText #-}

-- | Parse a 'KindID' from its string representation as a lazy 'ByteString'. It
-- is 'parseByteString' with concrete type.
parseByteString :: forall prefix
                 . (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
                => ByteString -> Either TypeIDError (KindID prefix)
parseByteString = KID.parseByteString

-- | Parse a 'KindID' from its 'String' representation, throwing an error when
-- the parsing fails. It is 'string2IDM' with concrete type.
parseStringM :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix), MonadIO m)
             => String -> m (KindID prefix)
parseStringM = KID.parseStringM
{-# INLINE parseStringM #-}

-- | Parse a 'KindID' from its string representation as a strict 'Text',
-- throwing an error when the parsing fails. It is 'text2IDM' with concrete
-- type. It is 'parseTextM' with concrete type.
parseTextM :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix), MonadIO m)
           => Text -> m (KindID prefix)
parseTextM = KID.parseTextM
{-# INLINE parseTextM #-}

-- | Parse a 'KindID' from its string representation as a lazy 'ByteString',
-- throwing an error when the parsing fails. It is 'byteString2IDM' with
-- concrete type. It is 'parseByteStringM' with concrete type.
parseByteStringM :: ( ToPrefix prefix
                    , ValidPrefix (PrefixSymbol prefix)
                    , MonadIO m )
                 => ByteString -> m (KindID prefix)
parseByteStringM = KID.parseByteStringM
{-# INLINE parseByteStringM #-}

-- | Convert a 'KindID' to a 'TypeID'.
toTypeID :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
         => KindID prefix -> TypeID
toTypeID = KID.toTypeID
{-# INLINE toTypeID #-}

-- | Convert a 'TypeID' to a 'KindID'.
fromTypeID :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
           => TypeID -> Maybe (KindID prefix)
fromTypeID = KID.fromTypeID
{-# INLINE fromTypeID #-}
