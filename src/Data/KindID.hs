{-# LANGUAGE ScopedTypeVariables #-}

module Data.KindID where

import           Control.Monad
import           Data.ByteString.Lazy (ByteString)
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.TypeID (TypeID)
import qualified Data.TypeID.Internal as TID
import qualified Data.TypeID as TID
import           Data.UUID.V7 (UUID)
import           GHC.TypeLits

newtype KindID (prefix :: Symbol) = KindID { getUUID :: UUID }

getPrefix :: forall prefix. KnownSymbol prefix => KindID prefix -> Text
getPrefix _ = T.pack $ symbolVal (Proxy @prefix)
{-# INLINE getPrefix #-}

toTypeID :: forall prefix. KnownSymbol prefix => KindID prefix -> TypeID
toTypeID kid = TID.TypeID (getPrefix kid) (getUUID kid)
{-# INLINE toTypeID #-}

fromTypeID :: forall prefix. KnownSymbol prefix
           => TypeID -> Maybe (KindID prefix)
fromTypeID tid = do
  guard (T.pack (symbolVal (Proxy @prefix)) == TID.getPrefix tid)
  pure $ KindID (TID.getUUID tid)
{-# INLINE fromTypeID #-}

toString :: forall prefix. KnownSymbol prefix => KindID prefix -> String
toString = TID.toString . toTypeID
{-# INLINE toString #-}

toText :: forall prefix. KnownSymbol prefix => KindID prefix -> Text
toText = TID.toText . toTypeID
{-# INLINE toText #-}

toByteString :: forall prefix. KnownSymbol prefix => KindID prefix -> ByteString
toByteString = TID.toByteString . toTypeID
{-# INLINE toByteString #-}

parseString :: forall prefix. KnownSymbol prefix
            => String -> Either TID.TypeIDError (KindID prefix)
parseString str = do
  tid <- TID.parseString str
  case fromTypeID tid of
    Nothing  -> Left $ TID.TypeIDErrorPrefixMismatch
                       (T.pack (symbolVal (Proxy @prefix)))
                       (TID.getPrefix tid)
    Just kid -> pure kid
{-# INLINE parseString #-}

parseText :: forall prefix. KnownSymbol prefix
          => Text -> Either TID.TypeIDError (KindID prefix)
parseText str = do
  tid <- TID.parseText str
  case fromTypeID tid of
    Nothing  -> Left $ TID.TypeIDErrorPrefixMismatch
                       (T.pack (symbolVal (Proxy @prefix)))
                       (TID.getPrefix tid)
    Just kid -> pure kid
{-# INLINE parseText #-}

parseByteString :: forall prefix. KnownSymbol prefix
                => ByteString -> Either TID.TypeIDError (KindID prefix)
parseByteString str = do
  tid <- TID.parseByteString str
  case fromTypeID tid of
    Nothing  -> Left $ TID.TypeIDErrorPrefixMismatch
                       (T.pack (symbolVal (Proxy @prefix)))
                       (TID.getPrefix tid)
    Just kid -> pure kid
{-# INLINE parseByteString #-}
