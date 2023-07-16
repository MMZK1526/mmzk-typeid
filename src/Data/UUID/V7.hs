-- |
-- Module      : Data.UUID.V7
-- License     : MIT
-- Maintainer  : mmzk1526@outlook.com
-- Portability : GHC
--
-- UUIDv7 implementation.
--
-- UUIDv7 is not currently present in the uuid package, therefore I have to
-- make a quick patch of my own.
--
-- Note that since the specification for v7 is not yet finalised, this module's
-- implementation may change in the future according to the potential
-- adjustments in the specification.
--
-- WARNING: The 'nil' re-export will be removed in the next major version.
--
module Data.UUID.V7
  (
  -- * Data type
    UUID
  , Word16
  , Word64
  -- * UUID generation
  , nil
  , genUUID
  , genUUID'
  , genUUIDs
  -- Validation
  , validate
  , validateWithTime
  -- * Miscellaneous helpers
  , getTime
  , getEpochMilli
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits
import qualified Data.ByteString.Lazy as BSL
import           Data.IORef
import           Data.Time.Clock.POSIX
import           Data.UUID.Types.Internal
import           System.Entropy
import           System.IO.Unsafe (unsafePerformIO)

-- | Generate a 'UUID'v7.
genUUID :: MonadIO m => m UUID
genUUID = head <$> genUUIDs 1
{-# INLINE genUUID #-}

-- | Generate a stateless 'UUID'v7.
--
-- It is faster than 'genUUID' but it is not guaranteed to be monotonically
-- increasing if multiple 'UUID's are generated at the same timestamp.
--
-- In use cases where the ordering is not important, this function is could be
-- preferred.
genUUID' :: MonadIO m => m UUID
genUUID' = do
  timestamp <- getEpochMilli
  entropy16 <- getEntropyWord16
  entropy64 <- getEntropyWord64
  let bs = runPut do
        fillTime timestamp
        fillVerAndRandA entropy16
        fillVarAndRandB entropy16 entropy64
  pure . uncurry UUID $ runGet (join (liftM2 (,)) getWord64be) bs

-- | Generate n 'UUID'v7s.
--
-- It tries its best to generate 'UUID's at the same timestamp, but it may not
-- be possible if we are asking too many 'UUID's at the same time.
--
-- It is guaranteed that the first 32768 'UUID's are generated at the same
-- timestamp.
genUUIDs :: MonadIO m => Word16 -> m [UUID]
genUUIDs 0 = pure []
genUUIDs n = liftIO do
  timestamp <- getEpochMilli
  -- We set the first bit of the entropy to 0 to ensure that there's enough
  -- room for incrementing the sequence number.
  entropy16 <- (.&. 0x7FFF) <$> getEntropyWord16
  -- Calculate the maximum number of slots we can use for the current timestamp
  -- before the sequence number overflows.
  let getMaxSlots num seqNo = if 0xFFFF - seqNo < num
        then (0xFFFF - seqNo, 0xFFFF)
        else (num, seqNo + num)
  -- Get the sequence number corresponding to the current timestamp and the
  -- number of UUIDs we can generate.
  (n', seqNo)  <- atomicModifyIORef __state__ $ \(ts, seqNo) -> if
    | ts < timestamp -> let (n', entropy16') = getMaxSlots n entropy16
                        in  ((timestamp, entropy16'), (n', entropy16 + 1))
    | ts > timestamp -> ((ts, seqNo), (0, 0))
    | otherwise      -> let (n', entropy16') = getMaxSlots n seqNo
                        in  ((timestamp, entropy16'), (n', seqNo + 1))
  -- If we can't generate any UUIDs, we try again, hoping that the timestamp
  -- has changed.
  if n' == 0
    then genUUIDs n
    else do
      uuids <- forM [0..(n' - 1)] $ \curN -> do
        entropy64 <- getEntropyWord64
        let bs = runPut do
              fillTime timestamp
              fillVerAndRandA (seqNo + curN)
              fillVarAndRandB (seqNo + curN) entropy64
        pure . uncurry UUID $ runGet (join (liftM2 (,)) getWord64be) bs
      if n' == n then pure uuids else (uuids ++) <$> genUUIDs (n - n')

-- | Validate the version and variant of the 'UUID'v7.
validate :: UUID -> Bool
validate (UUID w1 w2)
  = (w1 `shiftR` 12) .&. 0xF == 0x7 && (w2 `shiftR` 62) .&. 0x3 == 0x2
{-# INLINE validate #-}

-- | Validate the version and variant of the 'UUID'v7 as well as its timestamp
-- is no greater than the current time.
validateWithTime :: MonadIO m => UUID -> m Bool
validateWithTime uuid = do
  curTime <- getEpochMilli
  pure $ validate uuid && (getTime uuid <= curTime)
{-# INLINE validateWithTime #-}
  
-- | Get the current time in milliseconds since the Unix epoch.
getEpochMilli :: MonadIO m => m Word64
getEpochMilli = liftIO do
  t <- getPOSIXTime
  pure . round $ t * 1000
{-# INLINE getEpochMilli #-}

-- | Get the time field (unix_ts_ms) of a 'UUID'v7.
getTime :: UUID -> Word64
getTime (UUID w1 _) = w1 `shiftR` 16
{-# INLINE getTime #-}

-- | The global mutable state of (timestamp, sequence number).
--
-- The "NOINLINE" pragma is IMPORTANT! The logic would be flawed if it is
-- is inlined by its definition.
__state__ :: IORef (Word64, Word16)
__state__ = unsafePerformIO (newIORef (0, 0))
{-# NOINLINE __state__ #-}

-- | Fill in the 48-bit time field (unix_ts_ms) of a 'UUID'v7 with the given
-- time.
fillTime :: Word64 -> Put
fillTime timestamp = do
  let (_, p2, p1, p0) = splitWord64ToWord16s timestamp
  mapM_ putWord16be [p2, p1, p0]
{-# INLINE fillTime #-}

-- | Fill in the version and rand_a part of a 'UUID'v7 with the given sequence
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

-- | Fill in the variant and rand_b part of a 'UUID'v7 with the given sequence
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
  let seqNoRandB   = seqNo .&. 0xF
  let randBWithVar = fromIntegral (seqNoRandB .|. (0x2 `shiftL` 4))
  putWord64be $ (entropy .&. 0x3FFFFFFFFFFFFFF) .|. (randBWithVar `shiftL` 58)
{-# INLINE fillVarAndRandB #-}

splitWord64ToWord16s :: Word64 -> (Word16, Word16, Word16, Word16)
splitWord64ToWord16s n =
  let b0 = fromIntegral (n .&. 0xFFFF)
      b1 = fromIntegral ((n `shiftR` 16) .&. 0xFFFF)
      b2 = fromIntegral ((n `shiftR` 32) .&. 0xFFFF)
      b3 = fromIntegral ((n `shiftR` 48) .&. 0xFFFF)
  in (b3, b2, b1, b0)
{-# INLINE splitWord64ToWord16s #-}

getEntropyWord16 :: MonadIO m => m Word16
getEntropyWord16 = liftIO do
  bs <- BSL.fromStrict <$> getEntropy 2
  pure $ runGet getWord16host bs
{-# INLINE getEntropyWord16 #-}

getEntropyWord64 :: MonadIO m => m Word64
getEntropyWord64 = liftIO do
  bs <- BSL.fromStrict <$> getEntropy 8
  pure $ runGet getWord64host bs
{-# INLINE getEntropyWord64 #-}
