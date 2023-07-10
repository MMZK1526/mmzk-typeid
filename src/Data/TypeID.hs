-- |
-- Module      : Data.KindID
-- License     : MIT
-- Maintainer  : mmzk1526@outlook.com
-- Portability : GHC
--
-- An implementation of the typeid specification:
-- https://github.com/jetpack-io/typeid.
module Data.TypeID
  (
  -- * Data types
    TypeID
  , getPrefix
  , getUUID
  , getTime
  , TypeIDError(..)
  -- * TypeID generation
  , genTypeID
  , genTypeIDs
  , nil
  , decorate
  -- * Prefix validation
  , checkPrefix
  -- * Encoding & decoding
  , toString
  , toText
  , toByteString
  , parseString
  , parseText
  , parseByteString
  , parseStringWithPrefix
  , parseTextWithPrefix
  , parseByteStringWithPrefix
  ) where

import           Control.Exception
import           Control.Monad
import           Data.Aeson.Types hiding (Array, String)
import           Data.Bifunctor
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import           Data.Char
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding
import           Data.TypeID.Internal
import           Data.UUID.V7 (UUID(..))
import qualified Data.UUID.V7 as UUID
import           Data.Word

instance ToJSON TypeID where
  toJSON :: TypeID -> Value
  toJSON = toJSON . toText
  {-# INLINE toJSON #-}

instance FromJSON TypeID where
  parseJSON :: Value -> Parser TypeID
  parseJSON str = do
    s <- parseJSON str
    case parseText s of
      Left err  -> fail $ show err
      Right tid -> pure tid
  {-# INLINE parseJSON #-}

-- | Get the prefix of the 'TypeID'.
getPrefix :: TypeID -> Text
getPrefix = _getPrefix
{-# INLINE getPrefix #-}

-- | Get the 'UUID' of the 'TypeID'.
getUUID :: TypeID -> UUID
getUUID = _getUUID
{-# INLINE getUUID #-}

-- | Get the timestamp of the 'TypeID'.
getTime :: TypeID -> Word64
getTime (TypeID _ uuid) = UUID.getTime uuid
{-# INLINE getTime #-}

-- | Generate a new 'TypeID' from a prefix.
--
-- It throws a 'TypeIDError' if the prefix does not match the specification,
-- namely if it's longer than 63 characters or if it contains characters other
-- than lowercase latin letters.
genTypeID :: Text -> IO TypeID
genTypeID = fmap head . (`genTypeIDs` 1)
{-# INLINE genTypeID #-}

-- | Generate n 'TypeID's from a prefix.
--
-- It tries its best to generate 'TypeID's at the same timestamp, but it may not
-- be possible if we are asking too many 'UUID's at the same time.
--
-- It is guaranteed that the first 32768 'TypeID's are generated at the same
-- timestamp.
genTypeIDs :: Text -> Word16 -> IO [TypeID]
genTypeIDs prefix n = case checkPrefix prefix of
  Nothing  -> map (TypeID prefix) <$> UUID.genUUIDs n
  Just err -> throwIO err
{-# INLINE genTypeIDs #-}

-- | The nil 'TypeID'.
nil :: TypeID
nil = TypeID "" UUID.nil
{-# INLINE nil #-}

-- | Obtain a 'TypeID' from a prefix and a 'UUID'.
decorate :: Text -> UUID -> Either TypeIDError TypeID
decorate prefix uuid = case checkPrefix prefix of
  Nothing  -> Right $ TypeID prefix uuid
  Just err -> Left err
{-# INLINE decorate #-}

-- | Pretty-print a 'TypeID'.
toString :: TypeID -> String
toString (TypeID prefix uuid) = if T.null prefix
  then suffixEncode (UUID.unUUID uuid)
  else T.unpack prefix ++ "_" ++ suffixEncode (UUID.unUUID uuid)
{-# INLINE toString #-}

-- | Pretty-print a 'TypeID' to strict 'Text'.
toText :: TypeID -> Text
toText (TypeID prefix uuid) = if T.null prefix
  then T.pack (suffixEncode $ UUID.unUUID uuid)
  else prefix <> "_" <> T.pack (suffixEncode $ UUID.unUUID uuid)
{-# INLINE toText #-}

-- | Pretty-print a 'TypeID' to lazy 'ByteString'.
toByteString :: TypeID -> ByteString
toByteString = fromString . toString
{-# INLINE toByteString #-}

-- | Parse a 'TypeID' from its 'String' representation.
parseString :: String -> Either TypeIDError TypeID
parseString str = case span (/= '_') str of
  ("", _)              -> Left TypeIDExtraSeparator
  (_, "")              -> TypeID "" <$> decodeUUID bs
  (prefix, _ : suffix) -> do
    let prefix' = T.pack prefix
    let bs      = fromString suffix
    case checkPrefix prefix' of
      Nothing  -> TypeID prefix' <$> decodeUUID bs
      Just err -> Left err
  where
    bs = fromString str

-- | Parse a 'TypeID' from its string representation as a strict 'Text'.
parseText :: Text -> Either TypeIDError TypeID
parseText text = case second T.uncons $ T.span (/= '_') text of
  ("", _)                    -> Left TypeIDExtraSeparator
  (_, Nothing)               -> TypeID "" <$> decodeUUID bs
  (prefix, Just (_, suffix)) -> do
    let bs = BSL.fromStrict $ encodeUtf8 suffix
    case checkPrefix prefix of
      Nothing  -> TypeID prefix <$> decodeUUID bs
      Just err -> Left err
  where
    bs = BSL.fromStrict $ encodeUtf8 text

-- | Parse a 'TypeID' from its string representation as a lazy 'ByteString'.
parseByteString :: ByteString -> Either TypeIDError TypeID
parseByteString bs = case second BSL.uncons $ BSL.span (/= 95) bs of
  ("", _)                    -> Left TypeIDExtraSeparator
  (_, Nothing)               -> TypeID "" <$> decodeUUID bs
  (prefix, Just (_, suffix)) -> do
    let prefix' = decodeUtf8 $ BSL.toStrict prefix
    case checkPrefix prefix' of
      Nothing  -> TypeID prefix' <$> decodeUUID suffix
      Just err -> Left err

-- | Parse a 'TypeID' from the given prefix and the 'String' representation of a
-- suffix.
parseStringWithPrefix :: Text -> String -> Either TypeIDError TypeID
parseStringWithPrefix prefix str = case parseString str of
  Right (TypeID "" uuid) -> decorate prefix uuid
  Right (TypeID p  _)    -> Left $ TypeIDErrorAlreadyHasPrefix p
  Left err               -> Left err
{-# INLINE parseStringWithPrefix #-}

-- | Parse a 'TypeID' from the given prefix and the string representation of a
-- suffix as a strict 'Text'.
parseTextWithPrefix :: Text -> Text -> Either TypeIDError TypeID
parseTextWithPrefix prefix text = case parseText text of
  Right (TypeID "" uuid) -> decorate prefix uuid
  Right (TypeID p  _)    -> Left $ TypeIDErrorAlreadyHasPrefix p
  Left err               -> Left err
{-# INLINE parseTextWithPrefix #-}

-- | Parse a 'TypeID' from the given prefix and the string representation of a
-- suffix as a lazy 'ByteString'.
parseByteStringWithPrefix :: Text -> ByteString -> Either TypeIDError TypeID
parseByteStringWithPrefix prefix bs = case parseByteString bs of
  Right (TypeID "" uuid) -> decorate prefix uuid
  Right (TypeID p  _)    -> Left $ TypeIDErrorAlreadyHasPrefix p
  Left err               -> Left err
{-# INLINE parseByteStringWithPrefix #-}

-- | Check if the given prefix is a valid TypeID prefix.
checkPrefix :: Text -> Maybe TypeIDError
checkPrefix prefix
  | T.length prefix > 63 = Just $ TypeIDErrorPrefixTooLong (T.length prefix)
  | otherwise  
      = case T.uncons (T.dropWhile (liftM2 (&&) isLower isAscii) prefix) of
        Nothing     -> Nothing
        Just (c, _) -> Just $ TypeIDErrorPrefixInvalidChar c
{-# INLINE checkPrefix #-}
