module Data.UUID.V7 where
import Debug.Trace
import           Data.Binary.Put
import           Data.Bits
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import           Data.IORef
import           Data.Int
import           Data.Time.Clock.POSIX
import           System.IO.Unsafe (unsafePerformIO)

-- | The global mutable state of (timestamp, sequence number).
__state__ :: IORef (Int64, Int16)
__state__ = unsafePerformIO (newIORef (0, 0))
{-# NOINLINE __state__ #-}

-- | Fill in the 48-bit time field (unix_ts_ms) of a UUID V7 with the given
-- time.
fillTime :: Int64 -> Put
fillTime timestamp = do
  let (_, p1, p2, p3) = splitInt64 timestamp
  mapM_ putInt8 [p1, p2, p3]
{-# INLINE fillTime #-}

-- | Fill in the version and rand_a part of a UUID V7 with the given sequence
-- number.
--
-- The sequence number is a 16-bit integer, of which the first 12 bits are used
-- here in rand_a, and the last 4 bits are used in rand_b. The version is 7.
fillVerAndRandA :: Int16 -> Put
fillVerAndRandA seqNum = do
  let seqNumRandA  = seqNum `shiftR` 4
  let randAWithVer = seqNumRandA .|. (0x7 `shiftL` 12)
  putInt16be randAWithVer
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
fillVarAndRandB :: Int16 -> Int64 -> Put
fillVarAndRandB seqNum entropy = do
  let seqNumRandB  = seqNum .&. 0xF
  let randBWithVar = fromIntegral (seqNumRandB .|. (0x2 `shiftL` 4))
  putInt64be $ entropy .|. (randBWithVar `shiftL` 58)

-- | Get the current time in milliseconds since the Unix epoch.
getEpochMilli :: IO Int64
getEpochMilli = do
  t <- getPOSIXTime
  return $ round $ t * 1000
{-# INLINE getEpochMilli #-}

splitInt64 :: Int64 -> (Int8, Int8, Int8, Int8)
splitInt64 n =
  let b1 = fromIntegral (n .&. 0xFF)
      b2 = fromIntegral ((n `shiftR` 8) .&. 0xFF)
      b3 = fromIntegral ((n `shiftR` 16) .&. 0xFF)
      b4 = fromIntegral ((n `shiftR` 24) .&. 0xFF)
  in (b1, b2, b3, b4)
{-# INLINE splitInt64 #-}
