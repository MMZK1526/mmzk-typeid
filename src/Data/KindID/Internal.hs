{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Data.KindID.Internal
-- License     : MIT
-- Maintainer  : mmzk1526@outlook.com
-- Portability : GHC
--
module Data.KindID.Internal where

import           Control.Monad
import           Data.Aeson.Types hiding (String)
import           Data.ByteString.Lazy (ByteString)
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.KindID.Class
import           Data.TypeID.Class
import           Data.TypeID.Error
import           Data.TypeID.Internal (TypeID)
import qualified Data.TypeID.Internal as TID
import           Data.UUID.V7 (UUID)
import qualified Data.UUID.V7 as V7
import           Data.Word
import           GHC.TypeLits hiding (Text)

-- | A TypeID with the prefix encoded at type level.
--
-- It is dubbed 'KindID' because we the prefix here is a data kind rather than
-- a type.
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

-- | Generate a new 'KindID' from a prefix.
--
-- It throws a 'TypeIDError' if the prefix does not match the specification,
-- namely if it's longer than 63 characters or if it contains characters other
-- than lowercase latin letters.
genKindID :: forall prefix. (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
          => IO (KindID prefix)
genKindID = KindID <$> V7.genUUID
{-# INLINE genKindID #-}

-- | Generate n 'KindID's from a prefix.
--
-- It tries its best to generate 'KindID's at the same timestamp, but it may not
-- be possible if we are asking too many 'UUID's at the same time.
--
-- It is guaranteed that the first 32768 'KindID's are generated at the same
-- timestamp.
genKindIDs :: forall prefix
            . (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
           => Word16 -> IO [KindID prefix]
genKindIDs n = fmap KindID <$> V7.genUUIDs n
{-# INLINE genKindIDs #-}

-- | The nil 'KindID'.
nilKindID :: KindID ""
nilKindID = KindID V7.nil
{-# INLINE nilKindID #-}

-- | Obtain a 'KindID' from a prefix and a 'UUID'.
decorateKindID :: forall prefix
                . (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
               => UUID -> KindID prefix
decorateKindID = KindID

-- | Convert a 'KindID' to a 'TypeID'.
toTypeID :: forall prefix. (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
         => KindID prefix -> TypeID
toTypeID kid = TID.TypeID (getPrefix kid) (getUUID kid)
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

-- | Convert a 'TypeID' to a 'KindID'. If the actual prefix does not match
-- with the expected one as defined by the type, it does not complain and
-- produces a wrong 'KindID'.
unsafeFromTypeID :: forall prefix
                  . (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
                 => TypeID -> KindID prefix
unsafeFromTypeID tid = KindID (getUUID tid)

-- | Pretty-print a 'KindID'. It is 'id2String' with concrete type.
toString :: forall prefix. (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
         => KindID prefix -> String
toString = TID.toString . toTypeID
{-# INLINE toString #-}

-- | Pretty-print a 'KindID' to strict 'Text'. It is 'id2Text' with concrete
-- type.
toText :: forall prefix. (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
       => KindID prefix -> Text
toText = TID.toText . toTypeID
{-# INLINE toText #-}

-- | Pretty-print a 'KindID' to lazy 'ByteString'. It is 'id2ByteString' with
-- concrete type.
toByteString :: forall prefix
              . (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
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

-- | Parse a 'KindID' from its 'String' representation, but does not behave
-- correctly when parsing fails.
--
-- More specifically, if the prefix does not match, it will not complain and
-- produce the wrong 'KindID'. If there are other parse errors, it will crash.
unsafeParseString :: forall prefix
                   . (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
                  => String -> KindID prefix
unsafeParseString = unsafeFromTypeID . TID.unsafeParseString
{-# INLINE unsafeParseString #-}

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

-- | Parse a 'KindID' from its string representation as a strict 'Text', but
-- does not behave correctly when parsing fails.
--
-- More specifically, if the prefix does not match, it will not complain and
-- produce the wrong 'KindID'. If there are other parse errors, it will crash.
unsafeParseText :: forall prefix
                 . (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
                => Text -> KindID prefix
unsafeParseText = unsafeFromTypeID . TID.unsafeParseText

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

-- | Parse a 'KindID' from its string representation as a lazy 'ByteString', but
-- does not behave correctly when parsing fails.
--
-- More specifically, if the prefix does not match, it will not complain and
-- produce the wrong 'KindID'. If there are other parse errors, it will crash.
unsafeParseByteString :: forall prefix
                       . (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
                      => ByteString -> KindID prefix
unsafeParseByteString = unsafeFromTypeID . TID.unsafeParseByteString
{-# INLINE unsafeParseByteString #-}
