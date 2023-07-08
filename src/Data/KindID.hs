{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Similar to "Data.TypeID", but the type is statically determined in the type
-- level.
--
-- When using typeid, if we want to check if the type matches, we usually need
-- to get the prefix of the typeid and compare it with the desired prefix at
-- runtime. However, with Haskell's type system, we can do this at compile time
-- instead.
--
-- Of course, that would require the desired prefix to be known at compile time.
-- This is actually quite common, especially when we are using one prefix for
-- one table in the database.
--
-- For example, suppos we have a function that takes a typeid with the prefix
-- "user", it may have a signature like this:
-- @ f :: KindID "user" -> IO () @
--
-- Then if we try to pass in a typeid with prefix "post", the compiler will
-- complain, thus removing the runtime check and the associated overhead.
--
-- This module contains functions to generate and parse these type-level typeids
-- as well as conversion functions to and from the usual term-level typeids.
-- These functions are usually used with a type application, e.g.
-- > do
-- >   tid <- genKindID @"user"
-- >   ...
module Data.KindID
  (
  -- * Data types
    KindID(getUUID)
  , getPrefix
  -- * typeid generation
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
  -- type-level and term-level conversion
  , toTypeID
  , fromTypeID
  ) where

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
import qualified Data.UUID.V7 as V7
import           Data.Word
import           GHC.TypeLits

-- | A typeid with the prefix encoded at type level.
--
-- It is dubbed 'KindID' because we the prefix here is a data kind rather than
-- a type.
newtype KindID (prefix :: Symbol) = KindID { getUUID :: UUID }
  deriving (Eq, Ord, Show)

-- | Generate a new 'KindID' from a prefix.
--
-- It throws a 'TypeIDError' if the prefix does not match the specification,
-- namely if it's longer than 63 characters or if it contains characters other
-- than lowercase latin letters.
genKindID :: forall prefix. ValidPrefix prefix => IO (KindID prefix)
genKindID = KindID <$> V7.genUUID
{-# INLINE genKindID #-}

-- | Generate 'n' 'KindID's from a prefix.
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

getPrefix :: forall prefix. ValidPrefix prefix => KindID prefix -> Text
getPrefix _ = T.pack $ symbolVal (Proxy @prefix)
{-# INLINE getPrefix #-}

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

-- | Parse a 'KindID' from its String' representation.
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
