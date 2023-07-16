-- |
-- Module      : Data.TypeID.Internal
-- License     : MIT
-- Maintainer  : mmzk1526@outlook.com
-- Portability : GHC
--
module Data.TypeID.Internal where

import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.ST
import           Data.Aeson.Types hiding (Array, String)
import           Data.Array
import           Data.Array.ST
import           Data.Array.Unsafe (unsafeFreeze)
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bifunctor
import           Data.Bits
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import           Data.Char
import           Data.Hashable
import           Data.Proxy
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding
import           Data.TypeID.Class
import           Data.TypeID.Error
import           Data.UUID.Types.Internal (UUID(..))
import qualified Data.UUID.Types.Internal as UUID
import qualified Data.UUID.V7 as V7
import           Data.Word

-- | The constructor is not exposed to the public API to prevent generating
-- invalid @TypeID@s.
--
-- Note that the 'Show' instance is for debugging purposes only. To pretty-print
-- a 'TypeID', use 'toString', 'toText' or 'toByteString'. However, this
-- behaviour will be changed in the next major version as it is not useful. By
-- then, the 'Show' instance will be the same as 'toString'.
data TypeID = TypeID { _getPrefix :: Text
                     , _getUUID   :: UUID }
  deriving (Eq, Ord)

instance Show TypeID where
  show :: TypeID -> String
  show = toString
  {-# INLINE show #-}

instance Read TypeID where
  readsPrec :: Int -> String -> [(TypeID, String)]
  readsPrec _ str = case parseStringS str of
    Left _      -> []
    Right (x,y) -> [(x, y)]
  {-# INLINE readsPrec #-}

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

instance ToJSONKey TypeID where
  toJSONKey :: ToJSONKeyFunction TypeID
  toJSONKey = toJSONKeyText toText
  {-# INLINE toJSONKey #-}

instance FromJSONKey TypeID where
  fromJSONKey :: FromJSONKeyFunction TypeID
  fromJSONKey = FromJSONKeyTextParser \t -> case parseText t of
    Left err  -> fail $ show err
    Right tid -> pure tid
  {-# INLINE fromJSONKey #-}

instance Hashable TypeID where
  hashWithSalt :: Int -> TypeID -> Int
  hashWithSalt salt (TypeID prefix uuid)
    = salt `hashWithSalt` prefix `hashWithSalt` uuid
  {-# INLINE hashWithSalt #-}

-- | Get the prefix, 'UUID', and timestamp of a 'TypeID'.
instance IDType TypeID where
  getPrefix :: TypeID -> Text
  getPrefix = _getPrefix
  {-# INLINE getPrefix #-}

  getUUID :: TypeID -> UUID
  getUUID = _getUUID
  {-# INLINE getUUID #-}

  getTime :: TypeID -> Word64
  getTime = V7.getTime . getUUID
  {-# INLINE getTime #-}

-- | Conversion between 'TypeID' and 'String'/'Text'/'ByteString'.
instance IDConv TypeID where
  string2ID :: String -> Either TypeIDError TypeID
  string2ID = parseString
  {-# INLINE string2ID #-}

  text2ID :: Text -> Either TypeIDError TypeID
  text2ID = parseText
  {-# INLINE text2ID #-}

  byteString2ID :: ByteString -> Either TypeIDError TypeID
  byteString2ID = parseByteString
  {-# INLINE byteString2ID #-}

  id2String :: TypeID -> String
  id2String = toString
  {-# INLINE id2String #-}

  id2Text :: TypeID -> Text
  id2Text = toText
  {-# INLINE id2Text #-}

  id2ByteString :: TypeID -> ByteString
  id2ByteString = toByteString
  {-# INLINE id2ByteString #-}

-- | Generate 'TypeID's.
instance IDGen TypeID where
  type IDGenPrefix TypeID = 'Just Text

  genID_ :: MonadIO m => Proxy TypeID -> Text -> m TypeID
  genID_ _ = genTypeID
  {-# INLINE genID_ #-}

  genID'_ :: MonadIO m => Proxy TypeID -> Text -> m TypeID
  genID'_ _ = genTypeID'
  {-# INLINE genID'_ #-}

  genIDs_ :: MonadIO m => Proxy TypeID -> Text -> Word16 -> m [TypeID]
  genIDs_ _ = genTypeIDs
  {-# INLINE genIDs_ #-}

  decorate_ :: Proxy TypeID -> Text -> UUID -> Either TypeIDError TypeID
  decorate_ _ = decorateTypeID
  {-# INLINE decorate_ #-}

  checkID_ :: Proxy TypeID -> TypeID -> Maybe TypeIDError
  checkID_ _ = checkTypeID
  {-# INLINE checkID_ #-}

  checkIDWithEnv_ :: MonadIO m
                  => Proxy TypeID -> TypeID -> m (Maybe TypeIDError)
  checkIDWithEnv_ _ = checkTypeIDWithEnv
  {-# INLINE checkIDWithEnv_ #-}

-- | Generate a new 'TypeID' from a prefix.
--
-- It throws a 'TypeIDError' if the prefix does not match the specification,
-- namely if it's longer than 63 characters or if it contains characters other
-- than lowercase latin letters.
genTypeID :: MonadIO m => Text -> m TypeID
genTypeID = fmap head . (`genTypeIDs` 1)
{-# INLINE genTypeID #-}

-- | Generate a new 'TypeID' from a prefix based on statelesss 'UUID'v7.
--
-- See the documentation of 'V7.genUUID'' for more information.
genTypeID' :: MonadIO m => Text -> m TypeID
genTypeID' prefix = case checkPrefix prefix of
  Nothing  -> unsafeGenTypeID' prefix
  Just err -> liftIO $ throwIO err
{-# INLINE genTypeID' #-}

-- | Generate a list of 'TypeID's from a prefix.
--
-- It tries its best to generate 'TypeID's at the same timestamp, but it may not
-- be possible if we are asking too many 'UUID's at the same time.
--
-- It is guaranteed that the first 32768 'TypeID's are generated at the same
-- timestamp.
genTypeIDs :: MonadIO m => Text -> Word16 -> m [TypeID]
genTypeIDs prefix n = case checkPrefix prefix of
  Nothing  -> unsafeGenTypeIDs prefix n
  Just err -> liftIO $ throwIO err
{-# INLINE genTypeIDs #-}

-- | Generate a new 'TypeID' from a prefix, but without checking if the prefix
-- is valid.
unsafeGenTypeID :: MonadIO m => Text -> m TypeID
unsafeGenTypeID = fmap head . (`unsafeGenTypeIDs` 1)
{-# INLINE unsafeGenTypeID #-}

-- | Generate a new 'TypeID' from a prefix based on statelesss 'UUID'v7, but
-- without checking if the prefix is valid.
unsafeGenTypeID' :: MonadIO m => Text -> m TypeID
unsafeGenTypeID' prefix = TypeID prefix <$> V7.genUUID'
{-# INLINE unsafeGenTypeID' #-}

-- | Generate n 'TypeID's from a prefix, but without checking if the prefix is
-- valid.
--
-- It tries its best to generate 'TypeID's at the same timestamp, but it may not
-- be possible if we are asking too many 'UUID's at the same time.
--
-- It is guaranteed that the first 32768 'TypeID's are generated at the same
-- timestamp.
unsafeGenTypeIDs :: MonadIO m => Text -> Word16 -> m [TypeID]
unsafeGenTypeIDs prefix n = map (TypeID prefix) <$> V7.genUUIDs n
{-# INLINE unsafeGenTypeIDs #-}

-- | The nil 'TypeID'.
nilTypeID :: TypeID
nilTypeID = TypeID "" UUID.nil
{-# INLINE nilTypeID #-}
{-# DEPRECATED nilTypeID "Will be removed in the next major release." #-}

-- | Obtain a 'TypeID' from a prefix and a 'UUID'.
decorateTypeID :: Text -> UUID -> Either TypeIDError TypeID
decorateTypeID prefix uuid = case checkPrefix prefix of
  Nothing  -> Right $ TypeID prefix uuid
  Just err -> Left err
{-# INLINE decorateTypeID #-}

-- | Pretty-print a 'TypeID'. It is 'id2String' with concrete type.
toString :: TypeID -> String
toString (TypeID prefix (UUID w1 w2)) = if T.null prefix
  then suffixEncode bs
  else T.unpack prefix ++ "_" ++ suffixEncode bs
  where
    bs = runPut $ mapM_ putWord64be [w1, w2]
{-# INLINE toString #-}

-- | Pretty-print a 'TypeID' to strict 'Text'. It is 'id2Text' with concrete
-- type.
toText :: TypeID -> Text
toText (TypeID prefix (UUID w1 w2)) = if T.null prefix
  then T.pack (suffixEncode bs)
  else prefix <> "_" <> T.pack (suffixEncode bs)
  where
    bs = runPut $ mapM_ putWord64be [w1, w2]
{-# INLINE toText #-}

-- | Pretty-print a 'TypeID' to lazy 'ByteString'. It is 'id2ByteString' with
-- concrete type.
toByteString :: TypeID -> ByteString
toByteString = fromString . toString
{-# INLINE toByteString #-}

-- | Parse a 'TypeID' from its 'String' representation. It is 'string2ID' with
-- concrete type.
parseString :: String -> Either TypeIDError TypeID
parseString str = case parseStringS str of
  Left err        -> Left err
  Right (tid, "") -> Right tid
  _               -> Left TypeIDErrorUUIDError
{-# INLINE parseString #-}

parseStringS :: String -> Either TypeIDError (TypeID, String)
parseStringS str = case span (/= '_') str of
  ("", _)              -> Left TypeIDExtraSeparator
  (_, "")              -> do
    let (uuid, rem) = splitAt 26 str
        bs          = fromString uuid
    (, rem) . TypeID "" <$> decodeUUID bs
  (prefix, _ : suffix) -> do
    let prefix'     = T.pack prefix
        (uuid, rem) = splitAt 26 suffix
        bs          = fromString uuid
    case checkPrefix prefix' of
      Nothing  -> (, rem) . TypeID prefix' <$> decodeUUID bs
      Just err -> Left err

-- | Parse a 'TypeID' from its string representation as a strict 'Text'. It is
-- 'text2ID' with concrete type.
parseText :: Text -> Either TypeIDError TypeID
parseText text = case second T.uncons $ T.span (/= '_') text of
  ("", _)                    -> Left TypeIDExtraSeparator
  (_, Nothing)               -> TypeID "" <$> decodeUUID bs
  (prefix, Just (_, suffix)) -> do
    case checkPrefix prefix of
      Nothing  -> TypeID prefix
              <$> decodeUUID (BSL.fromStrict $ encodeUtf8 suffix)
      Just err -> Left err
  where
    bs = BSL.fromStrict $ encodeUtf8 text

-- | Parse a 'TypeID' from its string representation as a lazy 'ByteString'. It
-- is 'byteString2ID' with concrete type.
parseByteString :: ByteString -> Either TypeIDError TypeID
parseByteString bs = case second BSL.uncons $ BSL.span (/= 95) bs of
  ("", _)                    -> Left TypeIDExtraSeparator
  (_, Nothing)               -> TypeID "" <$> decodeUUID bs
  (prefix, Just (_, suffix)) -> do
    let prefix' = decodeUtf8 $ BSL.toStrict prefix
    case checkPrefix prefix' of
      Nothing  -> TypeID prefix' <$> decodeUUID suffix
      Just err -> Left err

-- | Parse a 'TypeID' from its string representation as a lazy 'ByteString',
-- but crashes when parsing fails.
unsafeParseByteString :: ByteString -> TypeID
unsafeParseByteString bs = case second BSL.uncons $ BSL.span (/= 95) bs of
  (_, Nothing)               -> TypeID "" $ unsafeDecodeUUID bs
  (prefix, Just (_, suffix)) -> TypeID (decodeUtf8 $ BSL.toStrict prefix)
                              . unsafeDecodeUUID $ suffix

-- | Check if the given prefix is a valid TypeID prefix.
checkPrefix :: Text -> Maybe TypeIDError
checkPrefix prefix
  | T.length prefix > 63 = Just $ TypeIDErrorPrefixTooLong (T.length prefix)
  | otherwise
      = case T.uncons (T.dropWhile (liftM2 (&&) isLower isAscii) prefix) of
        Nothing     -> Nothing
        Just (c, _) -> Just $ TypeIDErrorPrefixInvalidChar c
{-# INLINE checkPrefix #-}

-- | Check if the prefix is valid and the suffix 'UUID' has the correct v7
-- version and variant.
checkTypeID :: TypeID -> Maybe TypeIDError
checkTypeID (TypeID prefix uuid)
  = msum [ checkPrefix prefix
         , TypeIDErrorUUIDError <$ guard (V7.validate uuid) ]
{-# INLINE checkTypeID #-}

-- | Similar to 'checkTypeID', but also check if the suffix 'UUID' is
-- generated in the past.
checkTypeIDWithEnv :: MonadIO m => TypeID -> m (Maybe TypeIDError)
checkTypeIDWithEnv tid@(TypeID _ uuid)
  = fmap (checkTypeID tid `mplus`)
         (pure TypeIDErrorUUIDError <$ V7.validateWithTime uuid)
{-# INLINE checkTypeIDWithEnv #-}

-- | Parse a 'TypeID' from its 'String' representation, but crashes when
-- parsing fails.
unsafeParseString :: String -> TypeID
unsafeParseString str = case span (/= '_') str of
  (_, "")              -> TypeID "" $ unsafeDecodeUUID bs
  (prefix, _ : suffix) -> TypeID (T.pack prefix)
                        . unsafeDecodeUUID $ fromString suffix
  where
    bs = fromString str
{-# INLINE unsafeParseString #-}

-- | Parse a 'TypeID' from its string representation as a strict 'Text', but
-- crashes when parsing fails.
unsafeParseText :: Text -> TypeID
unsafeParseText text = case second T.uncons $ T.span (/= '_') text of
  (_, Nothing)               -> TypeID "" $ unsafeDecodeUUID bs
  (prefix, Just (_, suffix)) -> TypeID prefix . unsafeDecodeUUID
                              . BSL.fromStrict . encodeUtf8 $ suffix
  where
    bs = BSL.fromStrict $ encodeUtf8 text
{-# INLINE unsafeParseText #-}

-- The helpers below are verbatim translations from the official highly magical
-- Go implementation.

suffixEncode :: ByteString -> String
suffixEncode bs = (alphabet !) <$> runST do
  dest <- newArray_ (0, 25) :: ST s (STUArray s Int Word8)
  writeArray dest 0 $ (bs `BSL.index` 0 .&. 224) `shiftR` 5
  writeArray dest 1 $ bs `BSL.index` 0 .&. 31
  writeArray dest 2 $ (bs `BSL.index` 1 .&. 248) `shiftR` 3
  writeArray dest 3 $ ((bs `BSL.index` 1 .&. 7) `shiftL` 2) .|. ((bs `BSL.index` 2 .&. 192) `shiftR` 6)
  writeArray dest 4 $ (bs `BSL.index` 2 .&. 62) `shiftR` 1
  writeArray dest 5 $ ((bs `BSL.index` 2 .&. 1) `shiftL` 4) .|. ((bs `BSL.index` 3 .&. 240) `shiftR` 4)
  writeArray dest 6 $ ((bs `BSL.index` 3 .&. 15) `shiftL` 1) .|. ((bs `BSL.index` 4 .&. 128) `shiftR` 7)
  writeArray dest 7 $ (bs `BSL.index` 4 .&. 124) `shiftR` 2
  writeArray dest 8 $ ((bs `BSL.index` 4 .&. 3) `shiftL` 3) .|. ((bs `BSL.index` 5 .&. 224) `shiftR` 5)
  writeArray dest 9 $ bs `BSL.index` 5 .&. 31
  writeArray dest 10 $ (bs `BSL.index` 6 .&. 248) `shiftR` 3
  writeArray dest 11 $ ((bs `BSL.index` 6 .&. 7) `shiftL` 2) .|. ((bs `BSL.index` 7 .&. 192) `shiftR` 6)
  writeArray dest 12 $ (bs `BSL.index` 7 .&. 62) `shiftR` 1
  writeArray dest 13 $ ((bs `BSL.index` 7 .&. 1) `shiftL` 4) .|. ((bs `BSL.index` 8 .&. 240) `shiftR` 4)
  writeArray dest 14 $ ((bs `BSL.index` 8 .&. 15) `shiftL` 1) .|. ((bs `BSL.index` 9 .&. 128) `shiftR` 7)
  writeArray dest 15 $ (bs `BSL.index` 9 .&. 124) `shiftR` 2
  writeArray dest 16 $ ((bs `BSL.index` 9 .&. 3) `shiftL` 3) .|. ((bs `BSL.index` 10 .&. 224) `shiftR` 5)
  writeArray dest 17 $ bs `BSL.index` 10 .&. 31
  writeArray dest 18 $ (bs `BSL.index` 11 .&. 248) `shiftR` 3
  writeArray dest 19 $ ((bs `BSL.index` 11 .&. 7) `shiftL` 2) .|. ((bs `BSL.index` 12 .&. 192) `shiftR` 6)
  writeArray dest 20 $ (bs `BSL.index` 12 .&. 62) `shiftR` 1
  writeArray dest 21 $ ((bs `BSL.index` 12 .&. 1) `shiftL` 4) .|. ((bs `BSL.index` 13 .&. 240) `shiftR` 4)
  writeArray dest 22 $ ((bs `BSL.index` 13 .&. 15) `shiftL` 1) .|. ((bs `BSL.index` 14 .&. 128) `shiftR` 7)
  writeArray dest 23 $ (bs `BSL.index` 14 .&. 124) `shiftR` 2
  writeArray dest 24 $ ((bs `BSL.index` 14 .&. 3) `shiftL` 3) .|. ((bs `BSL.index` 15 .&. 224) `shiftR` 5)
  writeArray dest 25 $ bs `BSL.index` 15 .&. 31
  elems <$> unsafeFreeze dest
  where
    alphabet = listArray (0, 31) "0123456789abcdefghjkmnpqrstvwxyz"

suffixDecode :: ByteString -> ByteString
suffixDecode bs = BSL.pack $ runST do
  dest <- newArray_ (0, 15) :: ST s (STUArray s Int Word8)
  writeArray dest 0 $ ((table ! (bs `BSL.index` 0)) `shiftL` 5) .|. (table ! (bs `BSL.index` 1))
  writeArray dest 1 $ ((table ! (bs `BSL.index` 2)) `shiftL` 3) .|. ((table ! (bs `BSL.index` 3)) `shiftR` 2)
  writeArray dest 2 $ ((table ! (bs `BSL.index` 3)) `shiftL` 6) .|. ((table ! (bs `BSL.index` 4)) `shiftL` 1) .|. ((table ! (bs `BSL.index` 5)) `shiftR` 4)
  writeArray dest 3 $ ((table ! (bs `BSL.index` 5)) `shiftL` 4) .|. ((table ! (bs `BSL.index` 6)) `shiftR` 1)
  writeArray dest 4 $ ((table ! (bs `BSL.index` 6)) `shiftL` 7) .|. ((table ! (bs `BSL.index` 7)) `shiftL` 2) .|. ((table ! (bs `BSL.index` 8)) `shiftR` 3)
  writeArray dest 5 $ ((table ! (bs `BSL.index` 8)) `shiftL` 5) .|. (table ! (bs `BSL.index` 9))
  writeArray dest 6 $ ((table ! (bs `BSL.index` 10)) `shiftL` 3) .|. ((table ! (bs `BSL.index` 11)) `shiftR` 2)
  writeArray dest 7 $ ((table ! (bs `BSL.index` 11)) `shiftL` 6) .|. ((table ! (bs `BSL.index` 12)) `shiftL` 1) .|. ((table ! (bs `BSL.index` 13)) `shiftR` 4)
  writeArray dest 8 $ ((table ! (bs `BSL.index` 13)) `shiftL` 4) .|. ((table ! (bs `BSL.index` 14)) `shiftR` 1)
  writeArray dest 9 $ ((table ! (bs `BSL.index` 14)) `shiftL` 7) .|. ((table ! (bs `BSL.index` 15)) `shiftL` 2) .|. ((table ! (bs `BSL.index` 16)) `shiftR` 3)
  writeArray dest 10 $ ((table ! (bs `BSL.index` 16)) `shiftL` 5) .|. (table ! (bs `BSL.index` 17))
  writeArray dest 11 $ ((table ! (bs `BSL.index` 18)) `shiftL` 3) .|. (table ! (bs `BSL.index` 19)) `shiftR` 2
  writeArray dest 12 $ ((table ! (bs `BSL.index` 19)) `shiftL` 6) .|. ((table ! (bs `BSL.index` 20)) `shiftL` 1) .|. ((table ! (bs `BSL.index` 21)) `shiftR` 4)
  writeArray dest 13 $ ((table ! (bs `BSL.index` 21)) `shiftL` 4) .|. ((table ! (bs `BSL.index` 22)) `shiftR` 1)
  writeArray dest 14 $ ((table ! (bs `BSL.index` 22)) `shiftL` 7) .|. ((table ! (bs `BSL.index` 23)) `shiftL` 2) .|. ((table ! (bs `BSL.index` 24)) `shiftR` 3)
  writeArray dest 15 $ ((table ! (bs `BSL.index` 24)) `shiftL` 5) .|. (table ! (bs `BSL.index` 25))
  elems <$> unsafeFreeze dest

decodeUUID :: ByteString -> Either TypeIDError UUID
decodeUUID bs = do
  unless (BSL.length bs == 26) $ Left TypeIDErrorUUIDError
  unless (bs `BSL.index` 0 <= 55) $ Left TypeIDErrorUUIDError
  when (any ((== 0xFF) . (table !)) $ BSL.unpack bs) $ Left TypeIDErrorUUIDError
  pure $ unsafeDecodeUUID bs
{-# INLINE decodeUUID #-}

unsafeDecodeUUID :: ByteString -> UUID
unsafeDecodeUUID bs
  = uncurry UUID . runGet (join (liftM2 (,)) getWord64be) $ suffixDecode bs
{-# INLINE unsafeDecodeUUID #-}

table :: Array Word8 Word8
table = listArray (0, 255)
  [ 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF
  , 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF
  , 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF
  , 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF
  , 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0x01
  , 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0xFF, 0xFF
  , 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF
  , 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF
  , 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF
  , 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x0A, 0x0B, 0x0C
  , 0x0D, 0x0E, 0x0F, 0x10, 0x11, 0xFF, 0x12, 0x13, 0xFF, 0x14
  , 0x15, 0xFF, 0x16, 0x17, 0x18, 0x19, 0x1A, 0xFF, 0x1B, 0x1C
  , 0x1D, 0x1E, 0x1F, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF
  , 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF
  , 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF
  , 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF
  , 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF
  , 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF
  , 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF
  , 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF
  , 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF
  , 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF
  , 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF
  , 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF
  , 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF
  , 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF ]
