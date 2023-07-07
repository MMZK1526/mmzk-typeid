{-# LANGUAGE ScopedTypeVariables #-}

module Data.KindID where

import           Data.ByteString.Lazy (ByteString)
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.TypeID (TypeID)
import qualified Data.TypeID as TID
import           Data.UUID.V7 (UUID)
import           GHC.TypeLits

newtype KindID (prefix :: Symbol) = KindID { getUUID :: UUID }

getPrefix :: forall prefix. KnownSymbol prefix => KindID prefix -> Text
getPrefix _ = T.pack $ symbolVal (Proxy @prefix)

  -- fromTypeID :: TypeID -> Maybe (KindID prefix)

  -- toString :: KindID prefix -> String

  -- toText :: KindID prefix -> Text

  -- toByteString :: KindID prefix -> ByteString

  -- parseString :: String -> Maybe (KindID prefix)

  -- parseText :: Text -> Maybe (KindID prefix)

  -- parseByteString :: ByteString -> Maybe (KindID prefix)
