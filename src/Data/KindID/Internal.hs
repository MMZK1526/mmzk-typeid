{-# LANGUAGE FlexibleContexts #-}
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
import qualified Data.TypeID.Internal as TID
import           Data.UUID.Types.Internal (UUID(..))
import           Data.TypeID.V7 (TypeID)
import qualified Data.UUID.V7 as V7
import           Foreign
import           GHC.TypeLits (symbolVal)

-- | A TypeID with the prefix encoded at type level.
--
-- It is dubbed 'KindID' because the prefix here is a data kind rather than a
-- type.
newtype KindID prefix = KindID { _getUUID :: UUID }
  deriving (Eq, Ord)

instance (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
  => Show (KindID prefix) where
    show :: KindID prefix -> String
    show = toString
    {-# INLINE show #-}

instance (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
  => Read (KindID prefix) where
    readsPrec :: Int -> String -> [(KindID prefix, String)]
    readsPrec _ str = case TID.parseStringS str of
      Left _           -> []
      Right (tid, rem) -> case fromTypeID tid of
        Nothing  -> []
        Just kid -> [(kid, rem)]
    {-# INLINE readsPrec #-}

instance (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
  => ToJSON (KindID prefix) where
    toJSON :: KindID prefix -> Value
    toJSON = toJSON . toText
    {-# INLINE toJSON #-}

instance (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
  => FromJSON (KindID prefix) where
    parseJSON :: Value -> Parser (KindID prefix)
    parseJSON str = do
      s <- parseJSON str
      case parseText s of
        Left err  -> fail $ show err
        Right kid -> pure kid
    {-# INLINE parseJSON #-}

instance (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
  => ToJSONKey (KindID prefix) where
    toJSONKey :: ToJSONKeyFunction (KindID prefix)
    toJSONKey = toJSONKeyText toText
    {-# INLINE toJSONKey #-}

instance (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
  => FromJSONKey (KindID prefix) where
    fromJSONKey :: FromJSONKeyFunction (KindID prefix)
    fromJSONKey = FromJSONKeyTextParser \t -> case parseText t of
      Left err  -> fail $ show err
      Right kid -> pure kid
    {-# INLINE fromJSONKey #-}

-- | See The 'Binary' instance of 'TypeID'.
instance (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
  => Binary (KindID prefix) where
    put :: KindID prefix -> Put
    put = put . toTypeID
    {-# INLINE put #-}

    get :: Get (KindID prefix)
    get = do
      tid <- get :: Get TypeID
      case fromTypeID tid of
        Nothing  -> fail "Binary: Prefix mismatch"
        Just kid -> pure kid
    {-# INLINE get #-}

-- | Similar to the 'Binary' instance, but the 'UUID' is stored in host endian.
instance (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
  => Storable (KindID prefix) where
    sizeOf :: KindID prefix -> Int
    sizeOf = sizeOf . toTypeID
    {-# INLINE sizeOf #-}

    alignment :: KindID prefix -> Int
    alignment = alignment . toTypeID
    {-# INLINE alignment #-}

    peek :: Ptr (KindID prefix) -> IO (KindID prefix)
    peek ptr = do
      tid <- peek $ castPtr ptr :: IO TypeID
      case fromTypeID tid of
        Nothing  -> fail "Storable: Prefix mismatch"
        Just kid -> pure kid
    {-# INLINE peek #-}

    poke :: Ptr (KindID prefix) -> KindID prefix -> IO ()
    poke ptr = poke (castPtr ptr) . toTypeID
    {-# INLINE poke #-}

instance (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
  => Hashable (KindID prefix) where
    hashWithSalt :: Int -> KindID prefix -> Int
    hashWithSalt salt = hashWithSalt salt . toTypeID
    {-# INLINE hashWithSalt #-}

-- | Get the prefix, 'UUID', and timestamp of a 'KindID'.
instance (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
  => IDType (KindID prefix) where
    getPrefix :: KindID prefix -> Text
    getPrefix _ = T.pack (symbolVal (Proxy @(PrefixSymbol prefix)))
    {-# INLINE getPrefix #-}

    getUUID :: KindID prefix -> UUID
    getUUID = _getUUID
    {-# INLINE getUUID #-}

    getTime :: KindID prefix -> Word64
    getTime = V7.getTime . getUUID
    {-# INLINE getTime #-}

-- | Conversion between 'KindID' and 'String'/'Text'/'ByteString'.
instance (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
  => IDConv (KindID prefix) where
    string2ID :: String -> Either TypeIDError (KindID prefix)
    string2ID = parseString
    {-# INLINE string2ID #-}

    text2ID :: Text -> Either TypeIDError (KindID prefix)
    text2ID = parseText
    {-# INLINE text2ID #-}

    byteString2ID :: ByteString -> Either TypeIDError (KindID prefix)
    byteString2ID = parseByteString
    {-# INLINE byteString2ID #-}

    id2String :: KindID prefix -> String
    id2String = toString
    {-# INLINE id2String #-}

    id2Text :: KindID prefix -> Text
    id2Text = toText
    {-# INLINE id2Text #-}

    id2ByteString :: KindID prefix -> ByteString
    id2ByteString = toByteString
    {-# INLINE id2ByteString #-}

    unsafeString2ID :: String -> KindID prefix
    unsafeString2ID = unsafeParseString
    {-# INLINE unsafeString2ID #-}

    unsafeText2ID :: Text -> KindID prefix
    unsafeText2ID = unsafeParseText
    {-# INLINE unsafeText2ID #-}

    unsafeByteString2ID :: ByteString -> KindID prefix
    unsafeByteString2ID = unsafeParseByteString
    {-# INLINE unsafeByteString2ID #-}

-- | Generate 'KindID's.
instance (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
  => IDGen (KindID prefix) where
    type IDGenPrefix (KindID prefix) = 'Nothing

    genID_ :: MonadIO m => Proxy (KindID prefix) -> m (KindID prefix)
    genID_ _ = genKindID
    {-# INLINE genID_ #-}

    genID'_ :: MonadIO m => Proxy (KindID prefix) -> m (KindID prefix)
    genID'_ _ = genKindID'
    {-# INLINE genID'_ #-}

    genIDs_ :: MonadIO m => Proxy (KindID prefix) -> Word16 -> m [KindID prefix]
    genIDs_ _ = genKindIDs
    {-# INLINE genIDs_ #-}

    decorate_ :: Proxy (KindID prefix) -> UUID -> KindID prefix
    decorate_ _ = decorateKindID
    {-# INLINE decorate_ #-}

    checkID_ :: Proxy (KindID prefix) -> KindID prefix -> Maybe TypeIDError
    checkID_ _ = checkKindID
    {-# INLINE checkID_ #-}

    checkIDWithEnv_ :: MonadIO m
                    => Proxy (KindID prefix)
                    -> KindID prefix
                    -> m (Maybe TypeIDError)
    checkIDWithEnv_ _ = checkKindIDWithEnv
    {-# INLINE checkIDWithEnv_ #-}

-- | Generate a new 'KindID' from a prefix.
--
-- It throws a 'TypeIDError' if the prefix does not match the specification,
-- namely if it's longer than 63 characters or if it contains characters other
-- than lowercase latin letters.
genKindID :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix), MonadIO m)
          => m (KindID prefix)
genKindID = KindID <$> V7.genUUID
{-# INLINE genKindID #-}

-- | Generate a new 'KindID' from a prefix based on statelesss 'UUID'v7.
--
-- See the documentation of 'V7.genUUID'' for more information.
genKindID' :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix), MonadIO m)
           => m (KindID prefix)
genKindID' = KindID <$> V7.genUUID'
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
genKindIDs n = fmap KindID <$> V7.genUUIDs n
{-# INLINE genKindIDs #-}

-- | Obtain a 'KindID' from a prefix and a 'UUID'.
decorateKindID :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
               => UUID -> KindID prefix
decorateKindID = KindID
{-# INLINE decorateKindID #-}

-- | Convert a 'KindID' to a 'TypeID'.
toTypeID :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
         => KindID prefix -> TypeID
toTypeID kid = TID.TypeID' (getPrefix kid) (getUUID kid)
{-# INLINE toTypeID #-}

-- | Convert a 'TypeID' to a 'KindID'. If the actual prefix does not match
-- with the expected one as defined by the type, it returns @Nothing@.
fromTypeID :: forall prefix
            . (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
           => TypeID -> Maybe (KindID prefix)
fromTypeID tid = do
  guard (T.pack (symbolVal (Proxy @(PrefixSymbol prefix))) == getPrefix tid)
  pure $ KindID (getUUID tid)
{-# INLINE fromTypeID #-}

-- | Pretty-print a 'KindID'. It is 'id2String' with concrete type.
toString :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
         => KindID prefix -> String
toString = TID.toString . toTypeID
{-# INLINE toString #-}

-- | Pretty-print a 'KindID' to strict 'Text'. It is 'id2Text' with concrete
-- type.
toText :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
       => KindID prefix -> Text
toText = TID.toText . toTypeID
{-# INLINE toText #-}

-- | Pretty-print a 'KindID' to lazy 'ByteString'. It is 'id2ByteString' with
-- concrete type.
toByteString :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
             => KindID prefix -> ByteString
toByteString = TID.toByteString . toTypeID
{-# INLINE toByteString #-}

-- | Parse a 'KindID' from its 'String' representation. It is 'parseString'
-- with concrete type.
parseString :: forall prefix
             . (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
            => String -> Either TypeIDError (KindID prefix)
parseString str = do
  tid <- TID.parseString str
  case fromTypeID tid of
    Nothing  -> Left $ TypeIDErrorPrefixMismatch
                       (T.pack (symbolVal (Proxy @(PrefixSymbol prefix))))
                       (getPrefix tid)
    Just kid -> pure kid
{-# INLINE parseString #-}

-- | Parse a 'KindID' from its string representation as a strict 'Text'. It is
-- 'parseText' with concrete type.
parseText :: forall prefix. (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
          => Text -> Either TypeIDError (KindID prefix)
parseText str = do
  tid <- TID.parseText str
  case fromTypeID tid of
    Nothing  -> Left $ TypeIDErrorPrefixMismatch
                       (T.pack (symbolVal (Proxy @(PrefixSymbol prefix))))
                       (getPrefix tid)
    Just kid -> pure kid
{-# INLINE parseText #-}

-- | Parse a 'KindID' from its string representation as a lazy 'ByteString'. It
-- is 'parseByteString' with concrete type.
parseByteString :: forall prefix
                 . (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
                => ByteString -> Either TypeIDError (KindID prefix)
parseByteString str = do
  tid <- TID.parseByteString str
  case fromTypeID tid of
    Nothing  -> Left $ TypeIDErrorPrefixMismatch
                       (T.pack (symbolVal (Proxy @(PrefixSymbol prefix))))
                       (getPrefix tid)
    Just kid -> pure kid
{-# INLINE parseByteString #-}

-- | Parse a 'KindID' from its 'String' representation, throwing an error when
-- the parsing fails. It is 'string2IDM' with concrete type.
parseStringM :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix), MonadIO m)
             => String -> m (KindID prefix)
parseStringM = string2IDM
{-# INLINE parseStringM #-}

-- | Parse a 'KindID' from its string representation as a strict 'Text',
-- throwing an error when the parsing fails. It is 'text2IDM' with concrete
-- type.
parseTextM :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix), MonadIO m)
           => Text -> m (KindID prefix)
parseTextM = text2IDM
{-# INLINE parseTextM #-}

-- | Parse a 'KindID' from its string representation as a lazy 'ByteString',
-- throwing an error when the parsing fails. It is 'byteString2IDM' with
-- concrete type.
parseByteStringM :: ( ToPrefix prefix
                    , ValidPrefix (PrefixSymbol prefix)
                    , MonadIO m )
                 => ByteString
                 -> m (KindID prefix)
parseByteStringM = byteString2IDM
{-# INLINE parseByteStringM #-}

-- | Check if the prefix is valid and the suffix 'UUID' has the correct v7
-- version and variant.
checkKindID :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
            => KindID prefix -> Maybe TypeIDError
checkKindID = TID.checkTypeID . toTypeID
{-# INLINE checkKindID #-}

-- | Similar to 'checkKindID', but also checks if the suffix 'UUID' is
-- generated in the past.
checkKindIDWithEnv :: ( ToPrefix prefix
                      , ValidPrefix (PrefixSymbol prefix)
                      , MonadIO m )
                   => KindID prefix -> m (Maybe TypeIDError)
checkKindIDWithEnv = TID.checkTypeIDWithEnv . toTypeID
{-# INLINE checkKindIDWithEnv #-}

-- | Convert a 'TypeID' to a 'KindID'. If the actual prefix does not match
-- with the expected one as defined by the type, it does not complain and
-- produces a wrong 'KindID'.
unsafeFromTypeID :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
                 => TypeID -> KindID prefix
unsafeFromTypeID tid = KindID (getUUID tid)
{-# INLINE unsafeFromTypeID #-}

-- | Parse a 'KindID' from its 'String' representation, but does not behave
-- correctly when parsing fails.
--
-- More specifically, if the prefix does not match, it will not complain and
-- produce the wrong 'KindID'. If there are other parse errors, it will crash.
unsafeParseString :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
                  => String -> KindID prefix
unsafeParseString = unsafeFromTypeID . TID.unsafeParseString
{-# INLINE unsafeParseString #-}

-- | Parse a 'KindID' from its string representation as a strict 'Text', but
-- does not behave correctly when parsing fails.
--
-- More specifically, if the prefix does not match, it will not complain and
-- produce the wrong 'KindID'. If there are other parse errors, it will crash.
unsafeParseText :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
                => Text -> KindID prefix
unsafeParseText = unsafeFromTypeID . TID.unsafeParseText
{-# INLINE unsafeParseText #-}

-- | Parse a 'KindID' from its string representation as a lazy 'ByteString', but
-- does not behave correctly when parsing fails.
--
-- More specifically, if the prefix does not match, it will not complain and
-- produce the wrong 'KindID'. If there are other parse errors, it will crash.
unsafeParseByteString :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
                      => ByteString -> KindID prefix
unsafeParseByteString = unsafeFromTypeID . TID.unsafeParseByteString
{-# INLINE unsafeParseByteString #-}
