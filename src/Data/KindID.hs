{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Data.KindID
-- License     : MIT
-- Maintainer  : mmzk1526@outlook.com
-- Portability : GHC
--
-- Similar to "Data.TypeID", but the type is statically determined in the type
-- level.
--
-- When using TypeID, if we want to check if the type matches, we usually need
-- to get the prefix of the TypeID and compare it with the desired prefix at
-- runtime. However, with Haskell's type system, we can do this at compile time
-- instead. We call this TypeID with compile-time prefix a 'KindID'.
--
-- Of course, that would require the desired prefix to be known at compile time.
-- This is actually quite common, especially when we are using one prefix for
-- one table in the database.
--
-- For example, suppose we have a function that takes a 'KindID' with the prefix
-- "user", it may have a signature like this:
-- @ f :: KindID "user" -> IO () @
--
-- Then if we try to pass in a 'KindID' with the prefix "post", the compiler
-- will complain, thus removing the runtime check and the associated overhead.
--
-- All the prefixes are type-checked at compile time, so if we try to pass in
-- invalid prefixes, the compiler (again) will complain.
--
-- This module contains functions to generate and parse these type-level TypeIDs
-- as well as conversion functions to and from the usual term-level TypeIDs.
-- These functions are usually used with a type application, e.g.
--
-- > do
-- >   tid <- genKindID @"user"
-- >   ...
--
module Data.KindID
  (
  -- * Data types
    KindID
  , getPrefix
  , getUUID
  , getTime
  , ValidPrefix
  -- * KindID generation
  , genKindID
  , genKindIDs
  , nil
  , decorate
  -- * Encoding & decoding
  , toString
  , toText
  , toByteString
  , parseString
  , parseText
  , parseByteString
  -- * Type-level & term-level conversion
  , toTypeID
  , fromTypeID
  ) where

import           Control.Monad
import           Data.Aeson.Types hiding (String)
import           Data.ByteString.Lazy (ByteString)
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.KindID.Internal
import           Data.TypeID (TypeID, TypeIDError)
import qualified Data.TypeID as TID
import qualified Data.TypeID.Internal as TID
import           Data.UUID.V7 (UUID)
import qualified Data.UUID.V7 as V7
import           Data.Word
import           GHC.TypeLits hiding (Text)

instance ValidPrefix prefix => ToJSON (KindID prefix) where
  toJSON :: KindID prefix -> Value
  toJSON = toJSON . toText
  {-# INLINE toJSON #-}

instance ValidPrefix prefix => FromJSON (KindID prefix) where
  parseJSON :: Value -> Parser (KindID prefix)
  parseJSON str = do
    s <- parseJSON str
    case parseText s of
      Left err  -> fail $ show err
      Right kid -> pure kid
  {-# INLINE parseJSON #-}

-- | Generate a new 'KindID' from a prefix.
--
-- It throws a 'TypeIDError' if the prefix does not match the specification,
-- namely if it's longer than 63 characters or if it contains characters other
-- than lowercase latin letters.
genKindID :: forall prefix. ValidPrefix prefix => IO (KindID prefix)
genKindID = KindID <$> V7.genUUID
{-# INLINE genKindID #-}

-- | Generate n 'KindID's from a prefix.
--
-- It tries its best to generate 'KindID's at the same timestamp, but it may not
-- be possible if we are asking too many 'UUID's at the same time.
--
-- It is guaranteed that the first 32768 'KindID's are generated at the same
-- timestamp.
genKindIDs :: forall prefix. ValidPrefix prefix => Word16 -> IO [KindID prefix]
genKindIDs n = fmap KindID <$> V7.genUUIDs n
{-# INLINE genKindIDs #-}

-- | The nil 'KindID'.
nil :: KindID ""
nil = KindID V7.nil
{-# INLINE nil #-}

-- | Obtain a 'KindID' from a prefix and a 'UUID'.
decorate :: forall prefix. ValidPrefix prefix => UUID -> KindID prefix
decorate = KindID
{-# INLINE decorate #-}

-- | Get the prefix of the 'KindID'.
getPrefix :: forall prefix. ValidPrefix prefix => KindID prefix -> Text
getPrefix _ = T.pack $ symbolVal (Proxy @prefix)
{-# INLINE getPrefix #-}

-- | Get the 'UUID' of the 'KindID'.
getUUID :: forall prefix. ValidPrefix prefix => KindID prefix -> UUID
getUUID = _getUUID
{-# INLINE getUUID #-}

-- | Get the timestamp of the 'KindID'.
getTime :: forall prefix. ValidPrefix prefix => KindID prefix -> Word64
getTime = V7.getTime . getUUID
{-# INLINE getTime #-}

-- | Convert a 'KindID' to a 'TypeID'.
toTypeID :: forall prefix. ValidPrefix prefix => KindID prefix -> TypeID
toTypeID kid = TID.TypeID (getPrefix kid) (getUUID kid)
{-# INLINE toTypeID #-}

-- | Convert a 'TypeID' to a 'KindID'. If the actual prefix does not match
-- with the expected one as defined by the type, it returns @Nothing@.
fromTypeID :: forall prefix. ValidPrefix prefix
           => TypeID -> Maybe (KindID prefix)
fromTypeID tid = do
  guard (T.pack (symbolVal (Proxy @prefix)) == TID.getPrefix tid)
  pure $ KindID (TID.getUUID tid)
{-# INLINE fromTypeID #-}

-- | Pretty-print a 'KindID'.
toString :: forall prefix. ValidPrefix prefix => KindID prefix -> String
toString = TID.toString . toTypeID
{-# INLINE toString #-}

-- | Pretty-print a 'KindID' to strict 'Text'.
toText :: forall prefix. ValidPrefix prefix => KindID prefix -> Text
toText = TID.toText . toTypeID
{-# INLINE toText #-}

-- | Pretty-print a 'KindID' to lazy 'ByteString'.
toByteString :: forall prefix. ValidPrefix prefix => KindID prefix -> ByteString
toByteString = TID.toByteString . toTypeID
{-# INLINE toByteString #-}

-- | Parse a 'KindID' from its 'String' representation.
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

-- | Parse a 'KindID' from its string representation as a strict 'Text'.
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

-- | Parse a 'KindID' from its string representation as a lazy 'ByteString'.
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
