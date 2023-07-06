module Data.UUID.V7
  ( UUID(..)
  , genUUID
  , genUUIDs
  , getEpochMilli
  ) where

import           Control.Monad
import           Data.Array
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
  deriving (Eq, Ord)

instance Show UUID where
  show :: UUID -> String
  show (UUID bs)
    | BSL.length bs /= 16 = "<INVALID-UUID>"
    | otherwise           = word16ToHex b0
                          . word16ToHex b1
                          . (('-' :) . word16ToHex b2)
                          . (('-' :) . word16ToHex b3)
                          . (('-' :) . word16ToHex b4)
                          . (('-' :) . word16ToHex b5)
                          . word16ToHex b6
                          $ word16ToHex b7 ""
    where
      [b0, b1, b2, b3, b4, b5, b6, b7]
        = runGet (replicateM 8 getWord16be) bs
      hexTable
        = listArray (0, 15) "0123456789abcdef"
      word16ToHex w rem
       = let (q0, r0) = w `divMod` 16
             (q1, r1) = q0 `divMod` 16
             (q2, r2) = q1 `divMod` 16
             (q3, r3) = q2 `divMod` 16
         in  hexTable ! r3 : hexTable ! r2 : hexTable ! r1 : hexTable ! r0 : rem

-- | Generate a UUID V7.
genUUID :: IO UUID
genUUID = head <$> genUUIDs 1
{-# INLINE genUUID #-}

-- | Generate @n@ UUID V7s.
--
-- It tries its best to generate @UUIDs@ at the same timestamp, but it may not
-- be possible if we are asking too many @UUID@s at the same time.
--
-- It is guaranteed that the first 32768 @UUIDs@ are generated at the same
-- timestamp.
genUUIDs :: Word16 -> IO [UUID]
genUUIDs 0 = pure []
genUUIDs n = do
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
        pure . UUID $ runPut do
          fillTime timestamp
          fillVerAndRandA (seqNo + curN)
          fillVarAndRandB (seqNo + curN) entropy64
      if n' == n
        then pure uuids
        else (uuids ++) <$> genUUIDs (n - n')

-- | The global mutable state of (timestamp, sequence number).
--
-- The "NOINLINE" pragma is IMPORTANT! The logic would be flawed if @__state__@
-- is inlined by its definition.
__state__ :: IORef (Word64, Word16)
__state__ = unsafePerformIO (newIORef (0, 0))
{-# NOINLINE __state__ #-}

-- | Fill in the 48-bit time field (unix_ts_ms) of a UUID V7 with the given
-- time.
fillTime :: Word64 -> Put
fillTime timestamp = do
  let (_, p2, p1, p0) = splitWord64ToWord16s timestamp
  mapM_ putWord16be [p2, p1, p0]
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
  let seqNoRandB   = seqNo .&. 0xF
  let randBWithVar = fromIntegral (seqNoRandB .|. (0x2 `shiftL` 4))
  putWord64be $ (entropy .&. 0x3FFFFFFFFFFFFFF) .|. (randBWithVar `shiftL` 58)

-- | Get the current time in milliseconds since the Unix epoch.
getEpochMilli :: IO Word64
getEpochMilli = do
  t <- getPOSIXTime
  return $ round $ t * 1000
{-# INLINE getEpochMilli #-}

splitWord64ToWord16s :: Word64 -> (Word16, Word16, Word16, Word16)
splitWord64ToWord16s n =
  let b0 = fromIntegral (n .&. 0xFFFF)
      b1 = fromIntegral ((n `shiftR` 16) .&. 0xFFFF)
      b2 = fromIntegral ((n `shiftR` 32) .&. 0xFFFF)
      b3 = fromIntegral ((n `shiftR` 48) .&. 0xFFFF)
  in (b3, b2, b1, b0)
{-# INLINE splitWord64ToWord16s #-}

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
