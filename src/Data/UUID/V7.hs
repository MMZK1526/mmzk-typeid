module Data.UUID.V7 where

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

-- | Get the current time in milliseconds since the Unix epoch.
getEpochMilli :: IO Int64
getEpochMilli = do
  t <- getPOSIXTime
  return $ round $ t * 1000

-- | Fill in the 48-bit time field (unix_ts_ms) of a UUID V7 with the given
-- time.
fillTime :: Int64 -> Put
fillTime timestamp = do
  let (_, p1, p2, p3) = splitInt64 timestamp
  mapM_ putInt8 [p1, p2, p3]

splitInt64 :: Int64 -> (Int8, Int8, Int8, Int8)
splitInt64 n =
  let b1 = fromIntegral (n .&. 0xFF)
      b2 = fromIntegral ((n `shiftR` 8) .&. 0xFF)
      b3 = fromIntegral ((n `shiftR` 16) .&. 0xFF)
      b4 = fromIntegral ((n `shiftR` 24) .&. 0xFF)
  in (b1, b2, b3, b4)
