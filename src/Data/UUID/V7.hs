module Data.UUID.V7 where

import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import           Data.IORef
import           Data.Time.Clock.POSIX
import           Data.Word
import           System.Entropy
import           System.IO.Unsafe (unsafePerformIO)

newtype UUID = UUID { unUUID :: ByteString }
  deriving (Eq, Ord, Show)

-- | Generate @n@ UUID V7s.
--
-- It tries its best to generate UUIDs at the same timestamp, but it may not be
-- possible if we are asking too many UUIDs at the same time.
--
-- If @n@ is less than or equal to 0, an empty list is returned.
-- genUUIDs :: Int -> IO [UUID]
-- genUUIDs n | n <= 0 = pure []
-- genUUIDs n = do
--   timestamp <- getEpochMilli
--   -- Get the sequence number corresponding to the current timestamp.
--   -- If the current timestamp has not changed, we need to increment the
--   -- sequence number, unless it's already at the maximum value, in which case
--   -- we return "Nothing".
--   -- If the current timestamp increased, we reset the sequence number to the
--   -- pre-calculated entropy.
--   -- If the current timestamp decreased, it means another thread got ahead of
--   -- us, so we return "Nothing".
--   (n', seqNo)  <- atomicModifyIORef __state__ $ \(ts, seqNo) -> if
--     | ts < timestamp    -> ((timestamp, 0), (0, 0))
--     | ts > timestamp    -> ((ts, seqNo), (0, -1))
--     | seqNo == maxBound -> ((ts, seqNo), Nothing)
--     | otherwise         -> ((ts, seqNo + 1), Just (seqNo + 1))
--   -- When we get "Nothing", we try again, hopefully with a new timestamp.
--   case mSeqNo of
--     Nothing    -> genUUIDs n
--     Just seqNo -> do
--       undefined

-- | The global mutable state of (timestamp, sequence number).
__state__ :: IORef (Word64, Word16)
__state__ = unsafePerformIO (newIORef (0, 0))
{-# NOINLINE __state__ #-}

-- | Fill in the 48-bit time field (unix_ts_ms) of a UUID V7 with the given
-- time.
fillTime :: Word64 -> Put
fillTime timestamp = do
  let (_, p1, p2, p3) = splitWord64 timestamp
  mapM_ putWord8 [p1, p2, p3]
{-# INLINE fillTime #-}

-- | Fill in the version and rand_a part of a UUID V7 with the given sequence
-- number.
--
-- The sequence number is a 16-bit integer, of which the first 12 bits are used
-- here in rand_a, and the last 4 bits are used in rand_b. The version is 7.
fillVerAndRandA :: Word16 -> Put
fillVerAndRandA seqNo = do
  let seqNoRandA   = seqNo `shiftR` 4
  let randAWithVer = seqNoRandA .|. (0x7 `shiftL` 12)
  putWord16be randAWithVer
{-# INLINE fillVerAndRandA #-}

-- | Fill in the variant and rand_b part of a UUID V7 with the given sequence
-- number and random number. The variant is 2.
--
-- The sequence number is a 16-bit integer, of which the last 4 bits are used
-- here in rand_b while the first 12 bits are used in rand_a.
--
-- The random number is a 64-bit integer of which the last 58 bits are used
-- while the rest are replaced by the variant bits and the last 4 bits of the
-- sequence number.
fillVarAndRandB :: Word16 -> Word64 -> Put
fillVarAndRandB seqNo entropy = do
  let seqNoRandB  = seqNo .&. 0xF
  let randBWithVar = fromIntegral (seqNoRandB .|. (0x2 `shiftL` 4))
  putWord64be $ entropy .|. (randBWithVar `shiftL` 58)

-- | Get the current time in milliseconds since the Unix epoch.
getEpochMilli :: IO Word64
getEpochMilli = do
  t <- getPOSIXTime
  return $ round $ t * 1000
{-# INLINE getEpochMilli #-}

splitWord64 :: Word64 -> (Word8, Word8, Word8, Word8)
splitWord64 n =
  let b1 = fromIntegral (n .&. 0xFF)
      b2 = fromIntegral ((n `shiftR` 8) .&. 0xFF)
      b3 = fromIntegral ((n `shiftR` 16) .&. 0xFF)
      b4 = fromIntegral ((n `shiftR` 24) .&. 0xFF)
  in (b1, b2, b3, b4)
{-# INLINE splitWord64 #-}

getEntropyWord16 :: IO Word16
getEntropyWord16 = do
  bs <- BSL.fromStrict <$> getEntropy 2
  return $ runGet getWord16host bs
{-# INLINE getEntropyWord16 #-}

getEntropyWord64 :: IO Word64
getEntropyWord64 = do
  bs <- BSL.fromStrict <$> getEntropy 8
  return $ runGet getWord64host bs
{-# INLINE getEntropyWord64 #-}
