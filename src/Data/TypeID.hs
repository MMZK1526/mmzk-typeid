module Data.TypeID
  ( TypeID(..)
  , TypeIDError(..)
  , toString
  , toText
  , toByteString
  , parseString
  , parseText
  , parseByteString
  , genTypeID
  , genTypeIDs
  , checkPrefix
  ) where

import           Control.Exception
import           Control.Monad
import           Control.Monad.ST
import           Data.Array
import           Data.Array.ST
import           Data.Array.Unsafe
import           Data.Bifunctor
import           Data.Bits
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import           Data.Char
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding
import           Data.UUID.V7 (UUID(..))
import qualified Data.UUID.V7 as UUID
import           Data.Word

data TypeID = TypeID { getPrefix :: Text
                     , getUUID   :: UUID }
  deriving (Eq, Ord, Show)

data TypeIDError = TypeIDErrorPrefixTooLong Int
                 | TypeIDErrorPrefixInvalidChar Char
                 | TypeIDErrorUUIDError
  deriving (Eq, Ord)

instance Show TypeIDError where
  show :: TypeIDError -> String
  show (TypeIDErrorPrefixTooLong n)
    = concat ["Prefix with ", show n, " characters is too long!"]
  show (TypeIDErrorPrefixInvalidChar c)
    = concat ["Prefix contains invalid character ", show c, "!"]
  show TypeIDErrorUUIDError
    = "Invalid UUID part!"

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

-- | Parse a @TypeID@ from its @String@ representation.
parseString :: String -> Either TypeIDError TypeID
parseString str = case span (/= '_') str of
  (_, "")              -> TypeID "" <$> decodeUUID bs
  (prefix, _ : suffix) -> do
    let prefix' = T.pack prefix
    let bs      = fromString suffix
    unless (BSL.length bs == 26) $ Left TypeIDErrorUUIDError
    unless (bs `BSL.index` 0 <= 55) $ Left TypeIDErrorUUIDError
    when (any ((== 0xFF) . (table !)) $ BSL.unpack bs)
      $ Left TypeIDErrorUUIDError
    case checkPrefix prefix' of
      Nothing  -> TypeID prefix' <$> decodeUUID bs
      Just err -> Left err
    TypeID "" <$> decodeUUID bs
  where
    bs = fromString str

-- | Parse a @TypeID@ from its string representation as a strict @Text@.
parseText :: Text -> Either TypeIDError TypeID
parseText text = case second T.uncons $ T.span (/= '_') text of
  (_, Nothing)               -> TypeID "" <$> decodeUUID bs
  (prefix, Just (_, suffix)) -> do
    let bs = BSL.fromStrict $ encodeUtf8 suffix
    unless (BSL.length bs == 26) $ Left TypeIDErrorUUIDError
    unless (bs `BSL.index` 0 <= 55) $ Left TypeIDErrorUUIDError
    when (any ((== 0xFF) . (table !)) $ BSL.unpack bs)
      $ Left TypeIDErrorUUIDError
    case checkPrefix prefix of
      Nothing  -> TypeID prefix <$> decodeUUID bs
      Just err -> Left err
  where
    bs = BSL.fromStrict $ encodeUtf8 text

-- | Parse a @TypeID@ from its string representation as a lazy @ByteString@.
parseByteString :: ByteString -> Either TypeIDError TypeID
parseByteString bs = case second BSL.uncons $ BSL.span (/= 95) bs of
  (_, Nothing)               -> TypeID "" <$> decodeUUID bs
  (prefix, Just (_, suffix)) -> do
    let prefix' = decodeUtf8 $ BSL.toStrict prefix
    unless (BSL.length suffix == 26) $ Left TypeIDErrorUUIDError
    unless (suffix `BSL.index` 0 <= 55) $ Left TypeIDErrorUUIDError
    when (any ((== 0xFF) . (table !)) $ BSL.unpack suffix)
      $ Left TypeIDErrorUUIDError
    case checkPrefix prefix' of
      Nothing  -> TypeID prefix' <$> decodeUUID suffix
      Just err -> Left err

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

suffixDecode :: ByteString -> ByteString
suffixDecode bs = BSL.pack $ runST do
  dest <- newArray_ (0, 15) :: ST s (STUArray s Int Word8)
  writeArray dest 0 $ ((table ! (bs `BSL.index` 0)) `shiftL` 5) .|. (table ! (bs `BSL.index` 1))
  writeArray dest 1 $ ((table ! (bs `BSL.index` 2)) `shiftL` 3) .|. ((table ! (bs `BSL.index` 3)) `shiftR` 2)
  writeArray dest 2 $ ((table ! (bs `BSL.index` 3)) `shiftL` 6) .|. ((table ! (bs `BSL.index` 4)) `shiftL` 1) .|. ((table ! (bs `BSL.index` 5)) `shiftR` 4)
  writeArray dest 3 $ ((table ! (bs `BSL.index` 5)) `shiftL` 4) .|. ((table ! (bs `BSL.index` 6)) `shiftR` 1)
  writeArray dest 4 $ ((table ! (bs `BSL.index` 6)) `shiftL` 7) .|. ((table ! (bs `BSL.index` 7)) `shiftL` 2) .|. ((table ! (bs `BSL.index` 8)) `shiftR` 3)
  writeArray dest 5 $ ((table ! (bs `BSL.index` 8)) `shiftL` 5) .|. (table ! (bs `BSL.index` 9))
  writeArray dest 6 $ ((table ! (bs `BSL.index` 10)) `shiftL` 3) .|. ((table ! (bs `BSL.index` 11)) `shiftR` 2)
  writeArray dest 7 $ ((table ! (bs `BSL.index` 11)) `shiftL` 6) .|. ((table ! (bs `BSL.index` 12)) `shiftL` 1) .|. ((table ! (bs `BSL.index` 13)) `shiftR` 4)
  writeArray dest 8 $ ((table ! (bs `BSL.index` 13)) `shiftL` 4) .|. ((table ! (bs `BSL.index` 14)) `shiftR` 1)
  writeArray dest 9 $ ((table ! (bs `BSL.index` 14)) `shiftL` 7) .|. ((table ! (bs `BSL.index` 15)) `shiftL` 2) .|. ((table ! (bs `BSL.index` 16)) `shiftR` 3)
  writeArray dest 10 $ ((table ! (bs `BSL.index` 16)) `shiftL` 5) .|. (table ! (bs `BSL.index` 17))
  writeArray dest 11 $ ((table ! (bs `BSL.index` 18)) `shiftL` 3) .|. (table ! (bs `BSL.index` 19)) `shiftR` 2
  writeArray dest 12 $ ((table ! (bs `BSL.index` 19)) `shiftL` 6) .|. ((table ! (bs `BSL.index` 20)) `shiftL` 1) .|. ((table ! (bs `BSL.index` 21)) `shiftR` 4)
  writeArray dest 13 $ ((table ! (bs `BSL.index` 21)) `shiftL` 4) .|. ((table ! (bs `BSL.index` 22)) `shiftR` 1)
  writeArray dest 14 $ ((table ! (bs `BSL.index` 22)) `shiftL` 7) .|. ((table ! (bs `BSL.index` 23)) `shiftL` 2) .|. ((table ! (bs `BSL.index` 24)) `shiftR` 3)
  writeArray dest 15 $ ((table ! (bs `BSL.index` 24)) `shiftL` 5) .|. (table ! (bs `BSL.index` 25))
  elems <$> unsafeFreeze dest

decodeUUID :: ByteString -> Either TypeIDError UUID
decodeUUID bs = do
  unless (BSL.length bs == 26) $ Left TypeIDErrorUUIDError
  unless (bs `BSL.index` 0 <= 55) $ Left TypeIDErrorUUIDError
  when (any ((== 0xFF) . (table !)) $ BSL.unpack bs)
    $ Left TypeIDErrorUUIDError
  pure . UUID $ suffixDecode bs

table :: Array Word8 Word8
table = listArray (0, 255) 
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
