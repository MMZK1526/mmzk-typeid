{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Data.KindID.Internal
-- License     : MIT
-- Maintainer  : mmzk1526@outlook.com
-- Portability : GHC
--
module Data.KindID.Internal where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson.Types hiding (String)
import           Data.Binary
import           Data.ByteString.Lazy (ByteString)
import           Data.Hashable
import           Data.Proxy
import           Data.KindID.Class
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.TypeID.Class
import           Data.TypeID.Error
import           Data.TypeID.Internal (TypeID'(..))
import qualified Data.TypeID.Internal as TID
import           Data.UUID.Types.Internal (UUID(..))
import qualified Data.TypeID.V1.Unsafe as V1
import qualified Data.TypeID.V4.Unsafe as V4
import qualified Data.TypeID.V7.Unsafe as V7
import qualified Data.UUID.V7 as V7
import           Data.UUID.Versions
import           Foreign
import           GHC.TypeLits (symbolVal)
import           System.Random

-- | A TypeID with the prefix encoded at type level.
--
-- It is dubbed 'Data.KindID.V7.KindID' because the prefix here is a data kind
-- rather than a type.
newtype KindID' (version :: UUIDVersion) prefix = KindID' UUID
  deriving (Eq, Ord)

instance (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
  => Show (KindID' version prefix) where
    show :: KindID' version prefix -> String
    show = toString
    {-# INLINE show #-}

instance (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
  => Read (KindID' version prefix) where
    readsPrec :: Int -> String -> [(KindID' version prefix, String)]
    readsPrec _ str = case TID.parseStringS str of
      Left _           -> []
      Right (tid, rem) -> case fromTypeID tid of
        Nothing  -> []
        Just kid -> [(kid, rem)]
    {-# INLINE readsPrec #-}

instance (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
  => ToJSON (KindID' version prefix) where
    toJSON :: KindID' version prefix -> Value
    toJSON = toJSON . toText
    {-# INLINE toJSON #-}

instance (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
  => FromJSON (KindID' version prefix) where
    parseJSON :: Value -> Parser (KindID' version prefix)
    parseJSON str = do
      s <- parseJSON str
      case parseText s of
        Left err  -> fail $ show err
        Right kid -> pure kid
    {-# INLINE parseJSON #-}

instance (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
  => ToJSONKey (KindID' version prefix) where
    toJSONKey :: ToJSONKeyFunction (KindID' version prefix)
    toJSONKey = toJSONKeyText toText
    {-# INLINE toJSONKey #-}

instance (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
  => FromJSONKey (KindID' version prefix) where
    fromJSONKey :: FromJSONKeyFunction (KindID' version prefix)
    fromJSONKey = FromJSONKeyTextParser \t -> case parseText t of
      Left err  -> fail $ show err
      Right kid -> pure kid
    {-# INLINE fromJSONKey #-}

-- | See The 'Binary' instance of 'TypeID''.
instance (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
  => Binary (KindID' version prefix) where
    put :: KindID' version prefix -> Put
    put = put . toTypeID
    {-# INLINE put #-}

    get :: Get (KindID' version prefix)
    get = do
      tid <- get :: Get (TypeID' version)
      case fromTypeID tid of
        Nothing  -> fail "Binary: Prefix mismatch"
        Just kid -> pure kid
    {-# INLINE get #-}

-- | Similar to the 'Binary' instance, but the 'UUID' is stored in host endian.
instance (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
  => Storable (KindID' version prefix) where
    sizeOf :: KindID' version prefix -> Int
    sizeOf = sizeOf . toTypeID
    {-# INLINE sizeOf #-}

    alignment :: KindID' version prefix -> Int
    alignment = alignment . toTypeID
    {-# INLINE alignment #-}

    peek :: Ptr (KindID' version prefix) -> IO (KindID' version prefix)
    peek ptr = do
      tid <- peek $ castPtr ptr :: IO (TypeID' version)
      case fromTypeID tid of
        Nothing  -> fail "Storable: Prefix mismatch"
        Just kid -> pure kid
    {-# INLINE peek #-}

    poke :: Ptr (KindID' version prefix) -> KindID' version prefix -> IO ()
    poke ptr = poke (castPtr ptr) . toTypeID
    {-# INLINE poke #-}

instance (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
  => Hashable (KindID' version prefix) where
    hashWithSalt :: Int -> KindID' version prefix -> Int
    hashWithSalt salt = hashWithSalt salt . toTypeID
    {-# INLINE hashWithSalt #-}

-- | Get the prefix, 'UUID', and timestamp of a 'KindID''.
instance (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
  => IDType (KindID' version prefix) where
    getPrefix :: KindID' version prefix -> Text
    getPrefix _ = T.pack (symbolVal (Proxy @(PrefixSymbol prefix)))
    {-# INLINE getPrefix #-}

    getUUID :: KindID' version prefix -> UUID
    getUUID (KindID' uuid) = uuid
    {-# INLINE getUUID #-}

    getTime :: KindID' version prefix -> Word64
    getTime = V7.getTime . getUUID
    {-# INLINE getTime #-}

-- | Conversion between 'KindID'' and 'String'/'Text'/'ByteString'.
instance (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
  => IDConv (KindID' version prefix) where
    string2ID :: String -> Either TypeIDError (KindID' version prefix)
    string2ID = parseString
    {-# INLINE string2ID #-}

    text2ID :: Text -> Either TypeIDError (KindID' version prefix)
    text2ID = parseText
    {-# INLINE text2ID #-}

    byteString2ID :: ByteString -> Either TypeIDError (KindID' version prefix)
    byteString2ID = parseByteString
    {-# INLINE byteString2ID #-}

    id2String :: KindID' version prefix -> String
    id2String = toString
    {-# INLINE id2String #-}

    id2Text :: KindID' version prefix -> Text
    id2Text = toText
    {-# INLINE id2Text #-}

    id2ByteString :: KindID' version prefix -> ByteString
    id2ByteString = toByteString
    {-# INLINE id2ByteString #-}

    unsafeString2ID :: String -> KindID' version prefix
    unsafeString2ID = unsafeParseString
    {-# INLINE unsafeString2ID #-}

    unsafeText2ID :: Text -> KindID' version prefix
    unsafeText2ID = unsafeParseText
    {-# INLINE unsafeText2ID #-}

    unsafeByteString2ID :: ByteString -> KindID' version prefix
    unsafeByteString2ID = unsafeParseByteString
    {-# INLINE unsafeByteString2ID #-}

-- | Generate 'Data.KindID.V7.KindID's.
instance (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
  => IDGen (KindID' 'V7 prefix) where
    type IDGenPrefix (KindID' 'V7 prefix) = 'Nothing

    type IDGenReq (KindID' 'V7 prefix) r = r

    genID_ :: MonadIO m => Proxy (KindID' 'V7 prefix) -> m (KindID' 'V7 prefix)
    genID_ _ = genKindID
    {-# INLINE genID_ #-}

    genID'_ :: MonadIO m => Proxy (KindID' 'V7 prefix) -> m (KindID' 'V7 prefix)
    genID'_ _ = genKindID'
    {-# INLINE genID'_ #-}

    genIDs_ :: MonadIO m
            => Proxy (KindID' 'V7 prefix) -> Word16 -> m [KindID' 'V7 prefix]
    genIDs_ _ = genKindIDs
    {-# INLINE genIDs_ #-}

    decorate_ :: Proxy (KindID' 'V7 prefix) -> UUID -> KindID' 'V7 prefix
    decorate_ _ = decorateKindID
    {-# INLINE decorate_ #-}

    checkID_ :: Proxy (KindID' 'V7 prefix)
             -> KindID' 'V7 prefix
             -> Maybe TypeIDError
    checkID_ _ = checkKindID
    {-# INLINE checkID_ #-}

    checkIDWithEnv_ :: MonadIO m
                    => Proxy (KindID' 'V7 prefix)
                    -> KindID' 'V7 prefix
                    -> m (Maybe TypeIDError)
    checkIDWithEnv_ _ = checkKindIDWithEnv
    {-# INLINE checkIDWithEnv_ #-}

-- | Generate 'KindID'' ''V1's.
instance (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
  => IDGen (KindID' 'V1 prefix) where
    type IDGenPrefix (KindID' 'V1 prefix) = 'Nothing

    type IDGenReq (KindID' 'V1 prefix) r = r

    genID_ :: MonadIO m => Proxy (KindID' 'V1 prefix) -> m (KindID' 'V1 prefix)
    genID_ _ = genKindIDV1
    {-# INLINE genID_ #-}

    genIDs_ :: MonadIO m
            => Proxy (KindID' 'V1 prefix) -> Word16 -> m [KindID' 'V1 prefix]
    genIDs_ _ n
      = fmap KindID' <$> replicateM (fromIntegral n) (liftIO TID.nextUUID)
    {-# INLINE genIDs_ #-}

    decorate_ :: Proxy (KindID' 'V1 prefix) -> UUID -> KindID' 'V1 prefix
    decorate_ _ = decorateKindID
    {-# INLINE decorate_ #-}

    checkID_ :: Proxy (KindID' 'V1 prefix)
             -> KindID' 'V1 prefix
             -> Maybe TypeIDError
    checkID_ _ = checkKindIDV1
    {-# INLINE checkID_ #-}

-- | Generate 'KindID'' ''V4's.
instance (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
  => IDGen (KindID' 'V4 prefix) where
    type IDGenPrefix (KindID' 'V4 prefix) = 'Nothing

    type IDGenReq (KindID' 'V4 prefix) r = r

    genID_ :: MonadIO m => Proxy (KindID' 'V4 prefix) -> m (KindID' 'V4 prefix)
    genID_ _ = genKindIDV4
    {-# INLINE genID_ #-}

    genID'_ :: MonadIO m => Proxy (KindID' 'V4 prefix) -> m (KindID' 'V4 prefix)
    genID'_ _ = genKindIDV4'
    {-# INLINE genID'_ #-}

    genIDs_ :: MonadIO m
            => Proxy (KindID' 'V4 prefix) -> Word16 -> m [KindID' 'V4 prefix]
    genIDs_ p n = replicateM (fromIntegral n) (genID_ p)
    {-# INLINE genIDs_ #-}

    decorate_ :: Proxy (KindID' 'V4 prefix) -> UUID -> KindID' 'V4 prefix
    decorate_ _ = decorateKindID
    {-# INLINE decorate_ #-}

    checkID_ :: Proxy (KindID' 'V4 prefix)
             -> KindID' 'V4 prefix
             -> Maybe TypeIDError
    checkID_ _ = checkKindIDV4
    {-# INLINE checkID_ #-}

-- | Generate a new 'Data.KindID.V7.KindID' from a prefix.
--
-- It throws a 'TypeIDError' if the prefix does not match the specification,
-- namely if it's longer than 63 characters or if it contains characters other
-- than lowercase latin letters.
genKindID :: forall prefix m
           . ( ToPrefix prefix
             , ValidPrefix (PrefixSymbol prefix)
             , MonadIO m )
          => m (KindID' 'V7 prefix)
genKindID = unsafeFromTypeID 
        <$> V7.unsafeGenTypeID (T.pack $ symbolVal @(PrefixSymbol prefix) Proxy)
{-# INLINE genKindID #-}

-- | Generate a new 'Data.KindID.V7.KindID' from a prefix based on stateless
-- 'UUID'v7.
--
-- See the documentation of 'V7.genUUID'' for more information.
genKindID' :: forall prefix m
            . ( ToPrefix prefix
              , ValidPrefix (PrefixSymbol prefix)
              , MonadIO m )
           => m (KindID' 'V7 prefix)
genKindID' = fmap unsafeFromTypeID . V7.unsafeGenTypeID' . T.pack
           $ symbolVal @(PrefixSymbol prefix) Proxy
{-# INLINE genKindID' #-}

-- | Generate a list of 'Data.KindID.V7.KindID's from a prefix.
--
-- It tries its best to generate 'Data.KindID.V7.KindID's at the same timestamp,
-- but it may not be possible if we are asking too many 'UUID's at the same
-- time.
--
-- It is guaranteed that the first 32768 'Data.KindID.V7.KindID's are generated
-- at the same timestamp.
genKindIDs :: forall prefix m
            . ( ToPrefix prefix
              , ValidPrefix (PrefixSymbol prefix)
              , MonadIO m )
           => Word16 -> m [KindID' 'V7 prefix]
genKindIDs n = fmap (unsafeFromTypeID <$>) . flip V7.unsafeGenTypeIDs n . T.pack
             $ symbolVal @(PrefixSymbol prefix) Proxy
{-# INLINE genKindIDs #-}

-- | Generate a new 'KindID'' ''V1' from a prefix.
--
-- It throws a 'TypeIDError' if the prefix does not match the specification,
-- namely if it's longer than 63 characters or if it contains characters other
-- than lowercase latin letters.
genKindIDV1 :: forall prefix m
           . ( ToPrefix prefix
             , ValidPrefix (PrefixSymbol prefix)
             , MonadIO m )
          => m (KindID' 'V1 prefix)
genKindIDV1 = fmap unsafeFromTypeID . V1.unsafeGenTypeID . T.pack
            $ symbolVal @(PrefixSymbol prefix) Proxy
{-# INLINE genKindIDV1 #-}

-- | Generate a new 'KindID'' ''V4' from a prefix.
--
-- It throws a 'TypeIDError' if the prefix does not match the specification,
-- namely if it's longer than 63 characters or if it contains characters other
-- than lowercase latin letters.
genKindIDV4 :: forall prefix m
             . ( ToPrefix prefix
             , ValidPrefix (PrefixSymbol prefix)
             , MonadIO m )
            => m (KindID' 'V4 prefix)
genKindIDV4 = fmap unsafeFromTypeID . V4.unsafeGenTypeID . T.pack
            $ symbolVal @(PrefixSymbol prefix) Proxy
{-# INLINE genKindIDV4 #-}

-- | Generate a new 'KindID'' ''V4' from a prefix with insecure 'UUID'v4.
genKindIDV4' :: forall prefix m
              . ( ToPrefix prefix
                , ValidPrefix (PrefixSymbol prefix)
                , MonadIO m )
             => m (KindID' 'V4 prefix)
genKindIDV4' = fmap unsafeFromTypeID . V4.unsafeGenTypeID' . T.pack
             $ symbolVal @(PrefixSymbol prefix) Proxy
{-# INLINE genKindIDV4' #-}

-- | Obtain a 'KindID'' from a prefix and a 'UUID'.
decorateKindID :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
               => UUID -> KindID' version prefix
decorateKindID = KindID'
{-# INLINE decorateKindID #-}

-- | Convert a 'KindID'' to a 'TypeID''.
toTypeID :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
         => KindID' version prefix -> TypeID' version
toTypeID kid = TID.TypeID' (getPrefix kid) (getUUID kid)
{-# INLINE toTypeID #-}

-- | Convert a 'TypeID'' to a 'KindID''. If the actual prefix does not match
-- with the expected one as defined by the type, it returns @Nothing@.
fromTypeID :: forall version prefix
            . (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
           => TypeID' version -> Maybe (KindID' version prefix)
fromTypeID tid = do
  guard (T.pack (symbolVal (Proxy @(PrefixSymbol prefix))) == getPrefix tid)
  pure $ KindID' (getUUID tid)
{-# INLINE fromTypeID #-}

-- | Pretty-print a 'KindID''. It is 'id2String' with concrete type.
toString :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
         => KindID' version prefix -> String
toString = TID.toString . toTypeID
{-# INLINE toString #-}

-- | Pretty-print a 'KindID'' to strict 'Text'. It is 'id2Text' with concrete
-- type.
toText :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
       => KindID' version prefix -> Text
toText = TID.toText . toTypeID
{-# INLINE toText #-}

-- | Pretty-print a 'KindID'' to lazy 'ByteString'. It is 'id2ByteString' with
-- concrete type.
toByteString :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
             => KindID' version prefix -> ByteString
toByteString = TID.toByteString . toTypeID
{-# INLINE toByteString #-}

-- | Parse a 'KindID'' from its 'String' representation. It is 'parseString'
-- with concrete type.
parseString :: forall version prefix
             . (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
            => String -> Either TypeIDError (KindID' version prefix)
parseString str = do
  tid <- TID.parseString str
  case fromTypeID tid of
    Nothing  -> Left $ TypeIDErrorPrefixMismatch
                       (T.pack (symbolVal (Proxy @(PrefixSymbol prefix))))
                       (getPrefix tid)
    Just kid -> pure kid
{-# INLINE parseString #-}

-- | Parse a 'KindID'' from its string representation as a strict 'Text'. It is
-- 'parseText' with concrete type.
parseText :: forall version prefix
           . (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
          => Text -> Either TypeIDError (KindID' version prefix)
parseText str = do
  tid <- TID.parseText str
  case fromTypeID tid of
    Nothing  -> Left $ TypeIDErrorPrefixMismatch
                       (T.pack (symbolVal (Proxy @(PrefixSymbol prefix))))
                       (getPrefix tid)
    Just kid -> pure kid
{-# INLINE parseText #-}

-- | Parse a 'KindID'' from its string representation as a lazy 'ByteString'. It
-- is 'parseByteString' with concrete type.
parseByteString :: forall version prefix
                 . (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
                => ByteString -> Either TypeIDError (KindID' version prefix)
parseByteString str = do
  tid <- TID.parseByteString str
  case fromTypeID tid of
    Nothing  -> Left $ TypeIDErrorPrefixMismatch
                       (T.pack (symbolVal (Proxy @(PrefixSymbol prefix))))
                       (getPrefix tid)
    Just kid -> pure kid
{-# INLINE parseByteString #-}

-- | Parse a 'KindID'' from its 'String' representation, throwing an error when
-- the parsing fails. It is 'string2IDM' with concrete type.
parseStringM :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix), MonadIO m)
             => String -> m (KindID' version prefix)
parseStringM = string2IDM
{-# INLINE parseStringM #-}

-- | Parse a 'KindID'' from its string representation as a strict 'Text',
-- throwing an error when the parsing fails. It is 'text2IDM' with concrete
-- type.
parseTextM :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix), MonadIO m)
           => Text -> m (KindID' version prefix)
parseTextM = text2IDM
{-# INLINE parseTextM #-}

-- | Parse a 'KindID'' from its string representation as a lazy 'ByteString',
-- throwing an error when the parsing fails. It is 'byteString2IDM' with
-- concrete type.
parseByteStringM :: ( ToPrefix prefix
                    , ValidPrefix (PrefixSymbol prefix)
                    , MonadIO m )
                 => ByteString
                 -> m (KindID' version prefix)
parseByteStringM = byteString2IDM
{-# INLINE parseByteStringM #-}

-- | Check if the prefix is valid and the suffix 'UUID' has the correct v7
-- version and variant.
checkKindID :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
            => KindID' 'V7 prefix -> Maybe TypeIDError
checkKindID = TID.checkTypeID . toTypeID
{-# INLINE checkKindID #-}

-- | Similar to 'checkKindID', but also checks if the suffix 'UUID' is
-- generated in the past.
checkKindIDWithEnv :: ( ToPrefix prefix
                      , ValidPrefix (PrefixSymbol prefix)
                      , MonadIO m )
                   => KindID' 'V7 prefix -> m (Maybe TypeIDError)
checkKindIDWithEnv = TID.checkTypeIDWithEnv . toTypeID
{-# INLINE checkKindIDWithEnv #-}

-- | Check if the prefix is valid and the suffix 'UUID' has the correct v1
-- version and variant.
checkKindIDV1 :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
              => KindID' 'V1 prefix -> Maybe TypeIDError
checkKindIDV1 = TID.checkTypeIDV1 . toTypeID
{-# INLINE checkKindIDV1 #-}

-- | Check if the prefix is valid and the suffix 'UUID' has the correct v4
-- version and variant.
checkKindIDV4 :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
              => KindID' 'V4 prefix -> Maybe TypeIDError
checkKindIDV4 = TID.checkTypeIDV4 . toTypeID
{-# INLINE checkKindIDV4 #-}

-- | Convert a 'TypeID'' to a 'KindID''. If the actual prefix does not match
-- with the expected one as defined by the type, it does not complain and
-- produces a wrong 'KindID''.
unsafeFromTypeID :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
                 => TypeID' version -> KindID' version prefix
unsafeFromTypeID tid = KindID' (getUUID tid)
{-# INLINE unsafeFromTypeID #-}

-- | Parse a 'KindID'' from its 'String' representation, but does not behave
-- correctly when parsing fails.
--
-- More specifically, if the prefix does not match, it will not complain and
-- produce the wrong 'KindID''. If there are other parse errors, it will crash.
unsafeParseString :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
                  => String -> KindID' version prefix
unsafeParseString = unsafeFromTypeID . TID.unsafeParseString
{-# INLINE unsafeParseString #-}

-- | Parse a 'KindID'' from its string representation as a strict 'Text', but
-- does not behave correctly when parsing fails.
--
-- More specifically, if the prefix does not match, it will not complain and
-- produce the wrong 'KindID''. If there are other parse errors, it will crash.
unsafeParseText :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
                => Text -> KindID' version prefix
unsafeParseText = unsafeFromTypeID . TID.unsafeParseText
{-# INLINE unsafeParseText #-}

-- | Parse a 'KindID'' from its string representation as a lazy 'ByteString', but
-- does not behave correctly when parsing fails.
--
-- More specifically, if the prefix does not match, it will not complain and
-- produce the wrong 'KindID''. If there are other parse errors, it will crash.
unsafeParseByteString :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
                      => ByteString -> KindID' version prefix
unsafeParseByteString = unsafeFromTypeID . TID.unsafeParseByteString
{-# INLINE unsafeParseByteString #-}
