module Data.TypeID.Internal where

import           Control.Exception
import           Control.Monad
import           Control.Monad.ST
import           Data.Array
import           Data.Array.ST
import           Data.Array.Unsafe (unsafeFreeze)
import           Data.Bits
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import           Data.Text (Text)
import           Data.UUID.V7 (UUID(..))
import           Data.Word

-- | The constructor is not exposed to the public API to prevent generating
-- invalid @TypeID@s.
--
-- Note that the 'Show' instance is for debugging purposes only. To pretty-print
-- a 'TypeID', use 'toString', 'toText' or 'toByteString'.
data TypeID = TypeID { _getPrefix :: Text
                     , _getUUID   :: UUID }
  deriving (Eq, Ord, Show)

-- | Errors from parsing a @TypeID@.
data TypeIDError = TypeIDErrorPrefixTooLong Int
                 | TypeIDExtraSeparator
                 | TypeIDErrorPrefixInvalidChar Char
                 | TypeIDErrorAlreadyHasPrefix Text
                 | TypeIDErrorPrefixMismatch Text Text
                 | TypeIDErrorUUIDError
  deriving (Eq, Ord)

instance Show TypeIDError where
  show :: TypeIDError -> String
  show (TypeIDErrorPrefixTooLong n)
    = concat ["Prefix with ", show n, " characters is too long!"]
  show TypeIDExtraSeparator
    = "The underscore separator should not be present if the prefix is empty!"
  show (TypeIDErrorPrefixInvalidChar c)
    = concat ["Prefix contains invalid character ", show c, "!"]
  show (TypeIDErrorAlreadyHasPrefix prefix)
    = concat ["TypeID already has prefix ", show prefix, "!"]
  show (TypeIDErrorPrefixMismatch expPrefix actPrefix)
    = concat [ "Expected prefix ", show expPrefix, " but got "
             , show actPrefix, "!" ]
  show TypeIDErrorUUIDError
    = "Invalid UUID part!"
  {-# INLINE show #-}

instance Exception TypeIDError

-- The helpers below are verbatim translations from the official highly magical
-- Go implementation.

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
  when (any ((== 0xFF) . (table !)) $ BSL.unpack bs) $ Left TypeIDErrorUUIDError
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
