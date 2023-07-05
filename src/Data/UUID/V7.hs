module Data.UUID.V7 where

import           Data.Binary.Put
import           Data.Bits
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import           Data.Int
import           Data.Time.Clock.POSIX

-- | Get the current time in milliseconds since the Unix epoch.
getEpochMilli :: IO Int64
getEpochMilli = do
  t <- getPOSIXTime
  return $ round $ t * 1000

-- | Fill in the 48-bit time field (unix_ts_ms) of a UUID V7 with the given
-- time.
fillTime :: Int64 -> Put
fillTime timestamp = putInt64be $ shiftL timestamp 16
