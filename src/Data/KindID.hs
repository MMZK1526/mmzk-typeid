{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.KindID where

import           Control.Monad
import           Data.ByteString.Lazy (ByteString)
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Type.Bool
import           Data.Type.Equality
import           Data.Type.Ord
import           Data.TypeID (TypeID)
import qualified Data.TypeID.Internal as TID
import qualified Data.TypeID as TID
import           Data.UUID.V7 (UUID)
import           GHC.TypeLits

class KnownSymbol prefix => ValidPrefix (prefix :: Symbol)

instance ( KnownSymbol prefix
         , LengthSymbol prefix < 64
         , IsLowerSymbol prefix ~ 'True
         ) => ValidPrefix prefix

type family LengthSymbol (prefix :: Symbol) :: Nat where
  LengthSymbol prefix = LSUH (UnconsSymbol prefix)

-- | Length Symbol Uncons Helper.
type family LSUH (uncons :: Maybe (Char, Symbol)) :: Nat where
  LSUH 'Nothing        = 0
  LSUH ('Just '(c, s)) = 1 + LengthSymbol s

type family IsLowerChar (ch :: Char) :: Bool where
  IsLowerChar ch = Compare '`' ch == LT && Compare ch '{' == LT

type family IsLowerSymbol (prefix :: Symbol) :: Bool where
  IsLowerSymbol prefix = ILSUH (UnconsSymbol prefix)

-- | Is Lower Symbol Uncons Helper.
type family ILSUH (uncons :: Maybe (Char, Symbol)) :: Bool where
  ILSUH 'Nothing        = 'True
  ILSUH ('Just '(c, s)) = IsLowerChar c && IsLowerSymbol s

newtype KindID (prefix :: Symbol) = KindID { getUUID :: UUID }

getPrefix :: forall prefix. ValidPrefix prefix => KindID prefix -> Text
getPrefix _ = T.pack $ symbolVal (Proxy @prefix)
{-# INLINE getPrefix #-}

toTypeID :: forall prefix. ValidPrefix prefix => KindID prefix -> TypeID
toTypeID kid = TID.TypeID (getPrefix kid) (getUUID kid)
{-# INLINE toTypeID #-}

fromTypeID :: forall prefix. ValidPrefix prefix
           => TypeID -> Maybe (KindID prefix)
fromTypeID tid = do
  guard (T.pack (symbolVal (Proxy @prefix)) == TID.getPrefix tid)
  pure $ KindID (TID.getUUID tid)
{-# INLINE fromTypeID #-}

toString :: forall prefix. ValidPrefix prefix => KindID prefix -> String
toString = TID.toString . toTypeID
{-# INLINE toString #-}

toText :: forall prefix. ValidPrefix prefix => KindID prefix -> Text
toText = TID.toText . toTypeID
{-# INLINE toText #-}

toByteString :: forall prefix. ValidPrefix prefix => KindID prefix -> ByteString
toByteString = TID.toByteString . toTypeID
{-# INLINE toByteString #-}

parseString :: forall prefix. ValidPrefix prefix
            => String -> Either TID.TypeIDError (KindID prefix)
parseString str = do
  tid <- TID.parseString str
  case fromTypeID tid of
    Nothing  -> Left $ TID.TypeIDErrorPrefixMismatch
                       (T.pack (symbolVal (Proxy @prefix)))
                       (TID.getPrefix tid)
    Just kid -> pure kid
{-# INLINE parseString #-}

parseText :: forall prefix. ValidPrefix prefix
          => Text -> Either TID.TypeIDError (KindID prefix)
parseText str = do
  tid <- TID.parseText str
  case fromTypeID tid of
    Nothing  -> Left $ TID.TypeIDErrorPrefixMismatch
                       (T.pack (symbolVal (Proxy @prefix)))
                       (TID.getPrefix tid)
    Just kid -> pure kid
{-# INLINE parseText #-}

parseByteString :: forall prefix. ValidPrefix prefix
                => ByteString -> Either TID.TypeIDError (KindID prefix)
parseByteString str = do
  tid <- TID.parseByteString str
  case fromTypeID tid of
    Nothing  -> Left $ TID.TypeIDErrorPrefixMismatch
                       (T.pack (symbolVal (Proxy @prefix)))
                       (TID.getPrefix tid)
    Just kid -> pure kid
{-# INLINE parseByteString #-}
