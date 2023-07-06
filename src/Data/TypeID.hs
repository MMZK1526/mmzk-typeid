module Data.TypeID
  ( TypeID(..)
  , TypeIDError(..)
  , toString
  , toText
  , toByteString
  , genTypeID
  , genTypeIDs
  , checkPrefix
  ) where

import           Control.Exception
import           Control.Monad.ST
import           Data.Array
import           Data.Array.ST
import           Data.Array.Unsafe
import           Data.Bits
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import           Data.Char
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.UUID.V7 (UUID)
import qualified Data.UUID.V7 as UUID
import           Data.Word

data TypeID = TypeID { getPrefix :: Text
                     , getUUID   :: UUID }
  deriving (Eq, Ord, Show)

data TypeIDError = TypeIDErrorPrefixTooLong Int
                 | TypeIDErrorPrefixInvalidChar Char
  deriving (Eq, Ord)

instance Show TypeIDError where
  show :: TypeIDError -> String
  show (TypeIDErrorPrefixTooLong n)
    = concat ["Prefix with ", show n, " characters is too long!"]
  show (TypeIDErrorPrefixInvalidChar c)
    = concat ["Prefix contains invalid character ", show c, "!"]

instance Exception TypeIDError

-- | Pretty-print a @TypeID@.
toString :: TypeID -> String
toString (TypeID prefix uuid) = if T.null prefix
  then suffixEncode (UUID.unUUID uuid)
  else T.unpack prefix ++ "_" ++ suffixEncode (UUID.unUUID uuid)
{-# INLINE toString #-}

-- | Pretty-print a @TypeID@ to strict @Text@.
toText :: TypeID -> Text
toText (TypeID prefix uuid) = if T.null prefix
  then T.pack (suffixEncode $ UUID.unUUID uuid)
  else prefix <> "_" <> T.pack (suffixEncode $ UUID.unUUID uuid)
{-# INLINE toText #-}

-- | Pretty-print a @TypeID@ to lazy @ByteString@.
toByteString :: TypeID -> ByteString
toByteString = fromString . toString
{-# INLINE toByteString #-}

-- | Generates a new @TypeID@ from a prefix.
--
-- It throws a @TypeIDError@ if the prefix does not match the specification,
-- namely if it's longer than 63 characters or if it contains characters other
-- than lowercase latin letters.
genTypeID :: Text -> IO TypeID
genTypeID = fmap head . genTypeIDs 1
{-# INLINE genTypeID #-}

-- | Generates @n@ @TypeID@s from a prefix.
--
-- It tries its best to generate @TypeID@s at the same timestamp, but it may not
-- be possible if we are asking too many @UUID@s at the same time.
--
-- It is guaranteed that the first 32768 @TypeID@s are generated at the same
-- timestamp.
genTypeIDs :: Word16 -> Text -> IO [TypeID]
genTypeIDs n prefix = case checkPrefix prefix of
  Nothing  -> map (TypeID prefix) <$> UUID.genUUIDs n
  Just err -> throwIO err
{-# INLINE genTypeIDs #-}

-- | Check if the given prefix is a valid TypeID prefix.
checkPrefix :: Text -> Maybe TypeIDError
checkPrefix prefix
  | T.length prefix > 63 = Just $ TypeIDErrorPrefixTooLong (T.length prefix)
  | otherwise            = case T.uncons (T.dropWhile isLower prefix) of
      Nothing     -> Nothing
      Just (c, _) -> Just $ TypeIDErrorPrefixInvalidChar c
{-# INLINE checkPrefix #-}

-- The helpers below are verbatim and translation from the official highly
-- magical Go implementation.

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
