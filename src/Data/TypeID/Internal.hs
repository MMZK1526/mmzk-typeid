{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bifunctor
import           Data.Bits
import qualified Data.ByteString as BS
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import           Data.Char
import           Data.Data (Data)
import           Data.Hashable
import           Data.Proxy
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding
import           Data.Typeable (Typeable)
import           Data.TypeID.Class
import           Data.TypeID.Error
import           Data.UUID.Types.Internal (UUID(..))
import qualified Data.UUID.Types.Internal as UUID
import qualified Data.UUID.V1 as V1
import qualified Data.UUID.V4 as V4
import qualified Data.UUID.V5 as V5
import qualified Data.UUID.V7 as V7
import           Data.UUID.Versions
import           System.Random
import           Foreign

-- | This data type also supports 'Data.TypeID.V7.TypeID's with 'UUID' versions
-- other than v7.
--
--  The constructor is not exposed to the public API to prevent generating
-- invalid 'TypeID''s.
data TypeID' (version :: UUIDVersion) = TypeID' Text UUID
  deriving (Eq, Ord, Data, Typeable)

instance Show (TypeID' version) where
  show :: TypeID' version -> String
  show = toString
  {-# INLINE show #-}

instance Read (TypeID' version) where
  readsPrec :: Int -> String -> [(TypeID' version, String)]
  readsPrec _ str = case parseStringS str of
    Left _       -> []
    Right (x, y) -> [(x, y)]
  {-# INLINE readsPrec #-}

instance ToJSON (TypeID' version) where
  toJSON :: TypeID' version -> Value
  toJSON = toJSON . toText
  {-# INLINE toJSON #-}

instance FromJSON (TypeID' version) where
  parseJSON :: Value -> Parser (TypeID' version)
  parseJSON str = do
    s <- parseJSON str
    case parseText s of
      Left err  -> fail $ show err
      Right tid -> pure tid
  {-# INLINE parseJSON #-}

instance ToJSONKey (TypeID' version) where
  toJSONKey :: ToJSONKeyFunction (TypeID' version)
  toJSONKey = toJSONKeyText toText
  {-# INLINE toJSONKey #-}

instance FromJSONKey (TypeID' version) where
  fromJSONKey :: FromJSONKeyFunction (TypeID' version)
  fromJSONKey = FromJSONKeyTextParser \t -> case parseText t of
    Left err  -> fail $ show err
    Right tid -> pure tid
  {-# INLINE fromJSONKey #-}

-- | Since the specification does not formulate a concrete binary format, this
-- instance is based on the following custom format:
--
-- * The first 16 bytes are the suffix 'UUID' encoded in base32.
-- * The next byte is the length of the prefix encoded in a byte.
-- * The next bytes are the prefix, each letter taking 5 bits, mapping \'a\' to
--   1 and \'z\' to 26.
--
-- Note that the prefix and the 'UUID' is swapped compared to the string
-- representation, this is for the convenience of the use case where only the
-- suffix 'UUID' is required. Because of this, the sorting order may be
-- different from the string representation, but they are guaranteed to be the
-- same if the same prefix is used.
instance Binary (TypeID' version) where
  put :: TypeID' version -> Put
  put (TypeID' prefix uuid) = do
    put uuid
    let encodedPrefix = concat5BitInts . fmap (subtract 96) . BS.unpack
                      $ encodeUtf8 prefix
    putWord8 . fromIntegral $ length encodedPrefix
    forM_ encodedPrefix putWord8
  {-# INLINE put #-}

  get :: Get (TypeID' version)
  get = do
    uuid          <- get
    len           <- getWord8
    encodedPrefix <- separate5BitInts <$> replicateM (fromIntegral len) getWord8
    when (length encodedPrefix > 63) $ fail "Binary: Prefix too long"
    when (any (liftM2 (&&) (< 1) (> 25)) encodedPrefix)
         (fail "Binary: Invalid prefix")
    pure $ TypeID' (decodeUtf8 . BS.pack $ fmap (+ 96) encodedPrefix) uuid
  {-# INLINE get #-}

-- | Similar to the 'Binary' instance, but the 'UUID' is stored in host endian.
instance Storable (TypeID' version) where
  sizeOf :: TypeID' version -> Int
  sizeOf _ = 60
  {-# INLINE sizeOf #-}

  alignment :: TypeID' version -> Int
  alignment _ = 4
  {-# INLINE alignment #-}

  peek :: Ptr (TypeID' version) -> IO (TypeID' version)
  peek ptr = do
    uuid          <- peek (castPtr ptr :: Ptr UUID)
    len           <- fromIntegral <$> (peekByteOff ptr 16 :: IO Word8)
    encodedPrefix <- separate5BitInts
                 <$> forM [1..len] \ix -> peekByteOff @Word8 ptr (16 + ix)
    when (length encodedPrefix > 63) $ fail "Storable: Prefix too long"
    when (any (liftM2 (&&) (< 1) (> 25)) encodedPrefix)
         (fail "Storable: Invalid prefix")
    pure $ TypeID' (decodeUtf8 . BS.pack $ fmap (+ 96) encodedPrefix) uuid
  {-# INLINE peek #-}

  poke :: Ptr (TypeID' version) -> TypeID' version -> IO ()
  poke ptr (TypeID' prefix uuid) = do
    poke (castPtr ptr) uuid
    let encodedPrefix = concat5BitInts . fmap (subtract 96) . BS.unpack
                      $ encodeUtf8 prefix
    pokeByteOff @Word8 ptr 16 (fromIntegral $ length encodedPrefix)
    zipWithM_ (pokeByteOff ptr . (+ 16)) [1..] encodedPrefix
  {-# INLINE poke #-}

instance Hashable (TypeID' version) where
  hashWithSalt :: Int -> TypeID' version -> Int
  hashWithSalt salt (TypeID' prefix uuid)
    = salt `hashWithSalt` prefix `hashWithSalt` uuid
  {-# INLINE hashWithSalt #-}

-- | Get the prefix, 'UUID', and timestamp of a 'TypeID''.
instance IDType (TypeID' version) where
  getPrefix :: TypeID' version -> Text
  getPrefix (TypeID' prefix _) = prefix
  {-# INLINE getPrefix #-}

  getUUID :: TypeID' version -> UUID
  getUUID (TypeID' _ uuid) = uuid
  {-# INLINE getUUID #-}

  getTime :: TypeID' version -> Word64
  getTime = V7.getTime . getUUID
  {-# INLINE getTime #-}

-- | Conversion between 'TypeID'' and 'String'/'Text'/'ByteString'.
instance IDConv (TypeID' version) where
  string2ID :: String -> Either TypeIDError (TypeID' version)
  string2ID = parseString
  {-# INLINE string2ID #-}

  text2ID :: Text -> Either TypeIDError (TypeID' version)
  text2ID = parseText
  {-# INLINE text2ID #-}

  byteString2ID :: ByteString -> Either TypeIDError (TypeID' version)
  byteString2ID = parseByteString
  {-# INLINE byteString2ID #-}

  id2String :: TypeID' version -> String
  id2String = toString
  {-# INLINE id2String #-}

  id2Text :: TypeID' version -> Text
  id2Text = toText
  {-# INLINE id2Text #-}

  id2ByteString :: TypeID' version -> ByteString
  id2ByteString = toByteString
  {-# INLINE id2ByteString #-}

  unsafeString2ID :: String -> TypeID' version
  unsafeString2ID = unsafeParseString
  {-# INLINE unsafeString2ID #-}

  unsafeText2ID :: Text -> TypeID' version
  unsafeText2ID = unsafeParseText
  {-# INLINE unsafeText2ID #-}

  unsafeByteString2ID :: ByteString -> TypeID' version
  unsafeByteString2ID = unsafeParseByteString
  {-# INLINE unsafeByteString2ID #-}

-- | Generate 'Data.TypeIDs.V7.TypeIDs'.
instance IDGen (TypeID' 'V7) where
  type IDGenPrefix (TypeID' 'V7) = 'Just Text

  type IDGenReq (TypeID' 'V7) a = a

  genID_ :: MonadIO m => Proxy (TypeID' 'V7) -> Text -> m (TypeID' 'V7)
  genID_ _ = genTypeID
  {-# INLINE genID_ #-}

  genID'_ :: MonadIO m => Proxy (TypeID' 'V7) -> Text -> m (TypeID' 'V7)
  genID'_ _ = genTypeID'
  {-# INLINE genID'_ #-}

  genIDs_ :: MonadIO m
          => Proxy (TypeID' 'V7)
          -> Text
          -> Word16
          -> m [TypeID' 'V7]
  genIDs_ _ = genTypeIDs
  {-# INLINE genIDs_ #-}

  decorate_ :: Proxy (TypeID' 'V7)
            -> Text
            -> UUID
            -> Either TypeIDError (TypeID' 'V7)
  decorate_ _ = decorateTypeID
  {-# INLINE decorate_ #-}

  checkID_ :: Proxy (TypeID' 'V7) -> TypeID' 'V7 -> Maybe TypeIDError
  checkID_ _ = checkTypeID
  {-# INLINE checkID_ #-}

  checkIDWithEnv_ :: MonadIO m
                  => Proxy (TypeID' 'V7)
                  -> TypeID' 'V7
                  -> m (Maybe TypeIDError)
  checkIDWithEnv_ _ = checkTypeIDWithEnv
  {-# INLINE checkIDWithEnv_ #-}

-- | Generate 'TypeID'' ''V1's.
instance IDGen (TypeID' 'V1) where
  type IDGenPrefix (TypeID' 'V1) = 'Just Text

  type IDGenReq (TypeID' 'V1) a = a

  genID_ :: MonadIO m => Proxy (TypeID' 'V1) -> Text -> m (TypeID' 'V1)
  genID_ _ = genTypeIDV1
  {-# INLINE genID_ #-}

  genIDs_ :: MonadIO m
          => Proxy (TypeID' 'V1)
          -> Text
          -> Word16
          -> m [TypeID' 'V1]
  genIDs_ _ prefix n = case checkPrefix prefix of
    Nothing  -> map (TypeID' prefix)
            <$> replicateM (fromIntegral n) (liftIO nextUUID)
    Just err -> liftIO $ throwIO err
  {-# INLINE genIDs_ #-}

  decorate_ :: Proxy (TypeID' 'V1)
            -> Text
            -> UUID
            -> Either TypeIDError (TypeID' 'V1)
  decorate_ _ = decorateTypeID
  {-# INLINE decorate_ #-}

  checkID_ :: Proxy (TypeID' 'V1) -> TypeID' 'V1 -> Maybe TypeIDError
  checkID_ _ = checkTypeIDV1
  {-# INLINE checkID_ #-}

-- | Generate 'TypeID'' ''V4's.
instance IDGen (TypeID' 'V4) where
  type IDGenPrefix (TypeID' 'V4) = 'Just Text

  type IDGenReq (TypeID' 'V4) a = a

  genID_ :: MonadIO m => Proxy (TypeID' 'V4) -> Text -> m (TypeID' 'V4)
  genID_ _ = genTypeIDV4
  {-# INLINE genID_ #-}

  genID'_ :: MonadIO m => Proxy (TypeID' 'V4) -> Text -> m (TypeID' 'V4)
  genID'_ _ = genTypeIDV4'
  {-# INLINE genID'_ #-}

  genIDs_ :: MonadIO m
          => Proxy (TypeID' 'V4)
          -> Text
          -> Word16
          -> m [TypeID' 'V4]
  genIDs_ _ prefix n = case checkPrefix prefix of
    Nothing  -> map (TypeID' prefix)
            <$> replicateM (fromIntegral n) (liftIO V4.nextRandom)
    Just err -> liftIO $ throwIO err
  {-# INLINE genIDs_ #-}

  decorate_ :: Proxy (TypeID' 'V4)
            -> Text
            -> UUID
            -> Either TypeIDError (TypeID' 'V4)
  decorate_ _ = decorateTypeID
  {-# INLINE decorate_ #-}

  checkID_ :: Proxy (TypeID' 'V4) -> TypeID' 'V4 -> Maybe TypeIDError
  checkID_ _ = checkTypeIDV4
  {-# INLINE checkID_ #-}

-- | Generate 'TypeID'' ''V5's.
instance IDGen (TypeID' 'V5) where
  type IDGenPrefix (TypeID' 'V5) = 'Just Text

  type IDGenReq (TypeID' 'V5) r = UUID -> [Word8] -> r

  genID_ :: MonadIO m
         => Proxy (TypeID' 'V5) -> Text -> UUID -> [Word8] -> m (TypeID' 'V5)
  genID_ _ = genTypeIDV5
  {-# INLINE genID_ #-}

  genIDs_ :: MonadIO m
          => Proxy (TypeID' 'V5)
          -> Text
          -> UUID
          -> [Word8]
          -> Word16
          -> m [TypeID' 'V5]
  -- Apparently this function is useless...
  genIDs_ _ prefix ns obj n = case checkPrefix prefix of
    Nothing  -> replicateM (fromIntegral n)
              $ pure (TypeID' prefix $ V5.generateNamed ns obj)
    Just err -> liftIO $ throwIO err
  {-# INLINE genIDs_ #-}

  decorate_ :: Proxy (TypeID' 'V5)
            -> Text
            -> UUID
            -> Either TypeIDError (TypeID' 'V5)
  decorate_ _ = decorateTypeID
  {-# INLINE decorate_ #-}

  checkID_ :: Proxy (TypeID' 'V5) -> TypeID' 'V5 -> Maybe TypeIDError
  checkID_ _ = checkTypeIDV5
  {-# INLINE checkID_ #-}

-- | Generate a new 'Data.TypeID.V7.TypeID' from a prefix.
--
-- It throws a 'TypeIDError' if the prefix does not match the specification,
-- namely if it's longer than 63 characters or if it contains characters other
-- than lowercase latin letters.
genTypeID :: MonadIO m => Text -> m (TypeID' 'V7)
genTypeID = fmap head . (`genTypeIDs` 1)
{-# INLINE genTypeID #-}

-- | Generate a new 'Data.TypeID.V7.TypeID' from a prefix based on stateless
-- 'UUID'v7.
--
-- See the documentation of 'V7.genUUID'' for more information.
genTypeID' :: MonadIO m => Text -> m (TypeID' 'V7)
genTypeID' prefix = case checkPrefix prefix of
  Nothing  -> unsafeGenTypeID' prefix
  Just err -> liftIO $ throwIO err
{-# INLINE genTypeID' #-}

-- | Generate a list of 'Data.TypeID.V7.TypeID's from a prefix.
--
-- It tries its best to generate 'Data.TypeID.V7.TypeID's at the same timestamp,
-- but it may not be possible if we are asking too many 'UUID's at the same
-- time.
--
-- It is guaranteed that the first 32768 'Data.TypeID.V7.TypeID's are generated
-- at the same timestamp.
genTypeIDs :: MonadIO m => Text -> Word16 -> m [TypeID' 'V7]
genTypeIDs prefix n = case checkPrefix prefix of
  Nothing  -> unsafeGenTypeIDs prefix n
  Just err -> liftIO $ throwIO err
{-# INLINE genTypeIDs #-}

-- | Generate a new 'TypeID'' ''V1' from a prefix.
--
-- It throws a 'TypeIDError' if the prefix does not match the specification,
-- namely if it's longer than 63 characters or if it contains characters other
-- than lowercase latin letters.
genTypeIDV1 :: MonadIO m => Text -> m (TypeID' 'V1)
genTypeIDV1 prefix = case checkPrefix prefix of
  Nothing  -> unsafeGenTypeIDV1 prefix
  Just err -> liftIO $ throwIO err
{-# INLINE genTypeIDV1 #-}

-- | Generate a new 'TypeID'' ''V4' from a prefix.
--
-- It throws a 'TypeIDError' if the prefix does not match the specification,
-- namely if it's longer than 63 characters or if it contains characters other
-- than lowercase latin letters.
genTypeIDV4 :: MonadIO m => Text -> m (TypeID' 'V4)
genTypeIDV4 prefix = case checkPrefix prefix of
  Nothing  -> unsafeGenTypeIDV4 prefix
  Just err -> liftIO $ throwIO err
{-# INLINE genTypeIDV4 #-}

-- | Generate a new 'TypeID'' ''V4' from a prefix based on insecure 'UUID'v4.
genTypeIDV4' :: MonadIO m => Text -> m (TypeID' 'V4)
genTypeIDV4' prefix = case checkPrefix prefix of
  Nothing  -> unsafeGenTypeIDV4' prefix
  Just err -> liftIO $ throwIO err
{-# INLINE genTypeIDV4' #-}

-- | Generate a new 'TypeID'' ''V5' from a prefix, namespace, and object.
--
-- It throws a 'TypeIDError' if the prefix does not match the specification,
-- namely if it's longer than 63 characters or if it contains characters other
-- than lowercase latin letters.
genTypeIDV5 :: MonadIO m => Text -> UUID -> [Word8] -> m (TypeID' 'V5)
genTypeIDV5 prefix ns obj = case checkPrefix prefix of
  Nothing  -> pure $ unsafeGenTypeIDV5 prefix ns obj
  Just err -> throw err
{-# INLINE genTypeIDV5 #-}

-- | Obtain a 'TypeID'' from a prefix and a 'UUID'.
decorateTypeID :: Text -> UUID -> Either TypeIDError (TypeID' version)
decorateTypeID prefix uuid = case checkPrefix prefix of
  Nothing  -> Right $ TypeID' prefix uuid
  Just err -> Left err
{-# INLINE decorateTypeID #-}

-- | Pretty-print a 'TypeID''. It is 'id2String' with concrete type.
toString :: TypeID' version -> String
toString (TypeID' prefix (UUID w1 w2)) = if T.null prefix
  then suffixEncode bs
  else T.unpack prefix ++ "_" ++ suffixEncode bs
  where
    bs = runPut $ mapM_ putWord64be [w1, w2]
{-# INLINE toString #-}

-- | Pretty-print a 'TypeID'' to strict 'Text'. It is 'id2Text' with concrete
-- type.
toText :: TypeID' version -> Text
toText (TypeID' prefix (UUID w1 w2)) = if T.null prefix
  then T.pack (suffixEncode bs)
  else prefix <> "_" <> T.pack (suffixEncode bs)
  where
    bs = runPut $ mapM_ putWord64be [w1, w2]
{-# INLINE toText #-}

-- | Pretty-print a 'TypeID'' to lazy 'ByteString'. It is 'id2ByteString' with
-- concrete type.
toByteString :: TypeID' version -> ByteString
toByteString = fromString . toString
{-# INLINE toByteString #-}

-- | Parse a 'TypeID'' from its 'String' representation. It is 'string2ID' with
-- concrete type.
parseString :: String -> Either TypeIDError (TypeID' version)
parseString str = case parseStringS str of
  Left err        -> Left err
  Right (tid, "") -> Right tid
  _               -> Left TypeIDErrorUUIDError
{-# INLINE parseString #-}

parseStringS :: String -> Either TypeIDError (TypeID' version, String)
parseStringS str = case span (/= '_') str of
  ("", _)              -> Left TypeIDExtraSeparator
  (_, "")              -> do
    let (uuid, rem) = splitAt 26 str
        bs          = fromString uuid
    (, rem) . TypeID' "" <$> decodeUUID bs
  (prefix, _ : suffix) -> do
    let prefix'     = T.pack prefix
        (uuid, rem) = splitAt 26 suffix
        bs          = fromString uuid
    case checkPrefix prefix' of
      Nothing  -> (, rem) . TypeID' prefix' <$> decodeUUID bs
      Just err -> Left err

-- | Parse a 'TypeID'' from its string representation as a strict 'Text'. It is
-- 'text2ID' with concrete type.
parseText :: Text -> Either TypeIDError (TypeID' version)
parseText text = case second T.uncons $ T.span (/= '_') text of
  ("", _)                    -> Left TypeIDExtraSeparator
  (_, Nothing)               -> TypeID' ""
                            <$> decodeUUID (BSL.fromStrict $ encodeUtf8 text)
  (prefix, Just (_, suffix)) -> do
    case checkPrefix prefix of
      Nothing  -> TypeID' prefix
              <$> decodeUUID (BSL.fromStrict $ encodeUtf8 suffix)
      Just err -> Left err

-- | Parse a 'TypeID'' from its string representation as a lazy 'ByteString'. It
-- is 'byteString2ID' with concrete type.
parseByteString :: ByteString -> Either TypeIDError (TypeID' version)
parseByteString bs = case second BSL.uncons $ BSL.span (/= 95) bs of
  ("", _)                    -> Left TypeIDExtraSeparator
  (_, Nothing)               -> TypeID' "" <$> decodeUUID bs
  (prefix, Just (_, suffix)) -> do
    let prefix' = decodeUtf8 $ BSL.toStrict prefix
    case checkPrefix prefix' of
      Nothing  -> TypeID' prefix' <$> decodeUUID suffix
      Just err -> Left err

-- | Parse a 'TypeID'' from its 'String' representation, throwing an error when
-- the parsing fails. It is 'string2IDM' with concrete type.
parseStringM :: MonadIO m => String -> m (TypeID' version)
parseStringM = string2IDM
{-# INLINE parseStringM #-}

-- | Parse a 'TypeID'' from its string representation as a strict 'Text',
-- throwing an error when the parsing fails. It is 'text2IDM' with concrete
-- type.
parseTextM :: MonadIO m => Text -> m (TypeID' version)
parseTextM = text2IDM
{-# INLINE parseTextM #-}

-- | Parse a 'TypeID'' from its string representation as a lazy 'ByteString',
-- throwing an error when the parsing fails. It is 'byteString2IDM' with
-- concrete type.
parseByteStringM :: MonadIO m => ByteString -> m (TypeID' version)
parseByteStringM = byteString2IDM
{-# INLINE parseByteStringM #-}

-- | Check if the given prefix is a valid 'TypeID'' prefix.
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
checkTypeID :: TypeID' 'V7 -> Maybe TypeIDError
checkTypeID (TypeID' prefix uuid)
  = msum [ checkPrefix prefix
         , TypeIDErrorUUIDError <$ guard (not $ V7.validate uuid) ]
{-# INLINE checkTypeID #-}

-- | Check if the prefix is valid and the suffix 'UUID' has the correct v1
-- version and variant.
checkTypeIDV1 :: TypeID' 'V1 -> Maybe TypeIDError
checkTypeIDV1 (TypeID' prefix uuid)
  = msum [ checkPrefix prefix
         , TypeIDErrorUUIDError <$ guard (not $ validateWithVersion uuid V1) ]
{-# INLINE checkTypeIDV1 #-}

-- | Check if the prefix is valid and the suffix 'UUID' has the correct v4
-- version and variant.
checkTypeIDV4 :: TypeID' 'V4 -> Maybe TypeIDError
checkTypeIDV4 (TypeID' prefix uuid)
  = msum [ checkPrefix prefix
         , TypeIDErrorUUIDError <$ guard (not $ validateWithVersion uuid V4) ]
{-# INLINE checkTypeIDV4 #-}

-- | Check if the prefix is valid and the suffix 'UUID' has the correct v4
-- version and variant.
checkTypeIDV5 :: TypeID' 'V5 -> Maybe TypeIDError
checkTypeIDV5 (TypeID' prefix uuid)
  = msum [ checkPrefix prefix
         , TypeIDErrorUUIDError <$ guard (not $ validateWithVersion uuid V5) ]
{-# INLINE checkTypeIDV5 #-}

-- | Similar to 'checkTypeID', but also checks if the suffix 'UUID' is
-- generated in the past.
checkTypeIDWithEnv :: MonadIO m => TypeID' 'V7 -> m (Maybe TypeIDError)
checkTypeIDWithEnv tid@(TypeID' _ uuid)
  = fmap (checkTypeID tid `mplus`)
         ((TypeIDErrorUUIDError <$) . guard . not <$> V7.validateWithTime uuid)
{-# INLINE checkTypeIDWithEnv #-}

-- | Generate a new 'Data.TypeID.V7.TypeID' from a prefix, but without checking
-- if the prefix is valid.
unsafeGenTypeID :: MonadIO m => Text -> m (TypeID' 'V7)
unsafeGenTypeID = fmap head . (`unsafeGenTypeIDs` 1)
{-# INLINE unsafeGenTypeID #-}

-- | Generate a new 'TypeID'' ''V1' from a prefix, but without checking if the
-- prefix is valid.
unsafeGenTypeIDV1 :: MonadIO m => Text -> m (TypeID' 'V1)
unsafeGenTypeIDV1 prefix = TypeID' prefix <$> liftIO nextUUID
{-# INLINE unsafeGenTypeIDV1 #-}

-- | Generate a new 'TypeID'' ''V4' from a prefix, but without checking if the
-- prefix is valid.
unsafeGenTypeIDV4 :: MonadIO m => Text -> m (TypeID' 'V4)
unsafeGenTypeIDV4 prefix = TypeID' prefix <$> liftIO V4.nextRandom
{-# INLINE unsafeGenTypeIDV4 #-}

-- | Generate a new 'TypeID'' ''V5' from a prefix, namespace, and object, but
-- without checking if the prefix is valid.
unsafeGenTypeIDV5 :: Text -> UUID -> [Word8] -> TypeID' 'V5
unsafeGenTypeIDV5 prefix ns obj = TypeID' prefix (V5.generateNamed ns obj)
{-# INLINE unsafeGenTypeIDV5 #-}

-- | Generate a new 'Data.TypeID.V7.TypeID' from a prefix based on stateless
-- 'UUID'v7, but without checking if the prefix is valid.
unsafeGenTypeID' :: MonadIO m => Text -> m (TypeID' V7)
unsafeGenTypeID' prefix = TypeID' prefix <$> V7.genUUID'
{-# INLINE unsafeGenTypeID' #-}

-- | Generate a new 'TypeID'' ''V4' from a prefix based on insecure 'UUID'v4,
-- but without checking if the prefix is valid.
unsafeGenTypeIDV4' :: MonadIO m => Text -> m (TypeID' V4)
unsafeGenTypeIDV4' prefix = TypeID' prefix <$> liftIO randomIO
{-# INLINE unsafeGenTypeIDV4' #-}

-- | Generate n 'Data.TypeID.V7.TypeID's from a prefix, but without checking if
-- the prefix is valid.
--
-- It tries its best to generate 'Data.TypeID.V7.TypeID's at the same timestamp,
-- but it may not be possible if we are asking too many 'UUID's at the same
-- time.
--
-- It is guaranteed that the first 32768 'Data.TypeID.V7.TypeID's are generated
-- at the same timestamp.
unsafeGenTypeIDs :: MonadIO m => Text -> Word16 -> m [TypeID' V7]
unsafeGenTypeIDs prefix n = map (TypeID' prefix) <$> V7.genUUIDs n
{-# INLINE unsafeGenTypeIDs #-}

-- | Parse a 'TypeID'' from its 'String' representation, but crashes when
-- parsing fails.
unsafeParseString :: String -> TypeID' version
unsafeParseString str = case span (/= '_') str of
  (_, "")              -> TypeID' "" . unsafeDecodeUUID $ fromString str
  (prefix, _ : suffix) -> TypeID' (T.pack prefix)
                        . unsafeDecodeUUID $ fromString suffix
{-# INLINE unsafeParseString #-}

-- | Parse a 'TypeID'' from its string representation as a strict 'Text', but
-- crashes when parsing fails.
unsafeParseText :: Text -> TypeID' version
unsafeParseText text = case second T.uncons $ T.span (/= '_') text of
  (_, Nothing)               -> TypeID' "" . unsafeDecodeUUID
                              . BSL.fromStrict $ encodeUtf8 text
  (prefix, Just (_, suffix)) -> TypeID' prefix . unsafeDecodeUUID
                              . BSL.fromStrict . encodeUtf8 $ suffix
{-# INLINE unsafeParseText #-}

-- | Parse a 'TypeID'' from its string representation as a lazy 'ByteString',
-- but crashes when parsing fails.
unsafeParseByteString :: ByteString -> TypeID' version
unsafeParseByteString bs = case second BSL.uncons $ BSL.span (/= 95) bs of
  (_, Nothing)               -> TypeID' "" $ unsafeDecodeUUID bs
  (prefix, Just (_, suffix)) -> TypeID' (decodeUtf8 $ BSL.toStrict prefix)
                              . unsafeDecodeUUID $ suffix
{-# INLINE unsafeParseByteString #-}

concat5BitInts :: [Word8] -> [Word8]
concat5BitInts
  = reverse . toBytes
  . foldl (\(acc :: Integer) w -> acc `shiftL` 5 + fromIntegral w) 0
  where
    toBytes 0 = []
    toBytes x = fromIntegral (x .&. 0xFF) : toBytes (x `shiftR` 8)
{-# INLINE concat5BitInts #-}

separate5BitInts :: [Word8] -> [Word8]
separate5BitInts
  = reverse . toBytes
  . foldl (\(acc :: Integer) w -> acc `shiftL` 8 + fromIntegral w) 0
  where
    toBytes 0 = []
    toBytes x = fromIntegral (x .&. 0x1F) : toBytes (x `shiftR` 5)
{-# INLINE separate5BitInts #-}

-- | A helper for generating 'UUID'v1.
nextUUID :: IO UUID
nextUUID = V1.nextUUID >>= maybe nextUUID pure
{-# INLINE nextUUID #-}

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
  writeArray dest 0 $ ((base32Table ! (bs `BSL.index` 0)) `shiftL` 5) .|. (base32Table ! (bs `BSL.index` 1))
  writeArray dest 1 $ ((base32Table ! (bs `BSL.index` 2)) `shiftL` 3) .|. ((base32Table ! (bs `BSL.index` 3)) `shiftR` 2)
  writeArray dest 2 $ ((base32Table ! (bs `BSL.index` 3)) `shiftL` 6) .|. ((base32Table ! (bs `BSL.index` 4)) `shiftL` 1) .|. ((base32Table ! (bs `BSL.index` 5)) `shiftR` 4)
  writeArray dest 3 $ ((base32Table ! (bs `BSL.index` 5)) `shiftL` 4) .|. ((base32Table ! (bs `BSL.index` 6)) `shiftR` 1)
  writeArray dest 4 $ ((base32Table ! (bs `BSL.index` 6)) `shiftL` 7) .|. ((base32Table ! (bs `BSL.index` 7)) `shiftL` 2) .|. ((base32Table ! (bs `BSL.index` 8)) `shiftR` 3)
  writeArray dest 5 $ ((base32Table ! (bs `BSL.index` 8)) `shiftL` 5) .|. (base32Table ! (bs `BSL.index` 9))
  writeArray dest 6 $ ((base32Table ! (bs `BSL.index` 10)) `shiftL` 3) .|. ((base32Table ! (bs `BSL.index` 11)) `shiftR` 2)
  writeArray dest 7 $ ((base32Table ! (bs `BSL.index` 11)) `shiftL` 6) .|. ((base32Table ! (bs `BSL.index` 12)) `shiftL` 1) .|. ((base32Table ! (bs `BSL.index` 13)) `shiftR` 4)
  writeArray dest 8 $ ((base32Table ! (bs `BSL.index` 13)) `shiftL` 4) .|. ((base32Table ! (bs `BSL.index` 14)) `shiftR` 1)
  writeArray dest 9 $ ((base32Table ! (bs `BSL.index` 14)) `shiftL` 7) .|. ((base32Table ! (bs `BSL.index` 15)) `shiftL` 2) .|. ((base32Table ! (bs `BSL.index` 16)) `shiftR` 3)
  writeArray dest 10 $ ((base32Table ! (bs `BSL.index` 16)) `shiftL` 5) .|. (base32Table ! (bs `BSL.index` 17))
  writeArray dest 11 $ ((base32Table ! (bs `BSL.index` 18)) `shiftL` 3) .|. (base32Table ! (bs `BSL.index` 19)) `shiftR` 2
  writeArray dest 12 $ ((base32Table ! (bs `BSL.index` 19)) `shiftL` 6) .|. ((base32Table ! (bs `BSL.index` 20)) `shiftL` 1) .|. ((base32Table ! (bs `BSL.index` 21)) `shiftR` 4)
  writeArray dest 13 $ ((base32Table ! (bs `BSL.index` 21)) `shiftL` 4) .|. ((base32Table ! (bs `BSL.index` 22)) `shiftR` 1)
  writeArray dest 14 $ ((base32Table ! (bs `BSL.index` 22)) `shiftL` 7) .|. ((base32Table ! (bs `BSL.index` 23)) `shiftL` 2) .|. ((base32Table ! (bs `BSL.index` 24)) `shiftR` 3)
  writeArray dest 15 $ ((base32Table ! (bs `BSL.index` 24)) `shiftL` 5) .|. (base32Table ! (bs `BSL.index` 25))
  elems <$> unsafeFreeze dest

decodeUUID :: ByteString -> Either TypeIDError UUID
decodeUUID bs = do
  unless (BSL.length bs == 26) $ Left TypeIDErrorUUIDError
  unless (bs `BSL.index` 0 <= 55) $ Left TypeIDErrorUUIDError
  when (any ((== 0xFF) . (base32Table !)) $ BSL.unpack bs) $ Left TypeIDErrorUUIDError
  pure $ unsafeDecodeUUID bs
{-# INLINE decodeUUID #-}

unsafeDecodeUUID :: ByteString -> UUID
unsafeDecodeUUID
  = uncurry UUID . runGet (join (liftM2 (,)) getWord64be) . suffixDecode
{-# INLINE unsafeDecodeUUID #-}

base32Table :: Array Word8 Word8
base32Table = listArray (0, 255)
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
