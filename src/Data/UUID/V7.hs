-- | UUIDv7 implementation.
--
-- UUIDv7 is not currently present in the uuid package, therefore I have to
-- make a quick patch of my own. In the future I will try to add uuid as a
-- dependency and try to use the same interface.
--
-- Note that since the specification for v7 is not yet finalised, this module's
-- implementation may change in the future according to the potential
-- adjustments in the specification.
module Data.UUID.V7
  ( 
  -- * Data type
    UUID(..)
  -- * UUID generation
  , nil
  , genUUID
  , genUUIDs
  -- * Encoding & decoding
  , parseString
  , parseText
  , parseByteString
  , toString
  , toText
  , toByteString
  -- * Miscellaneous helper
  , getEpochMilli
  ) where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Data.Aeson.Types
import           Data.Array
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import           Data.IORef
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding
import           Data.Time.Clock.POSIX
import           Data.Word
import           System.Entropy
import           System.IO.Unsafe (unsafePerformIO)

newtype UUID = UUID { unUUID :: ByteString }
  deriving (Eq, Ord, Show)

instance ToJSON UUID where
  toJSON :: UUID -> Value
  toJSON = toJSON . toString
  {-# INLINE toJSON #-}

instance FromJSON UUID where
  parseJSON :: Value -> Parser UUID
  parseJSON str = do
    s <- parseJSON str
    case parseString s of
      Nothing   -> fail "Invalid UUID"
      Just uuid -> pure uuid
  {-# INLINE parseJSON #-}

-- | Pretty-print a 'UUID'v7. 
toString :: UUID -> String
toString (UUID bs)
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

-- | Pretty-print a 'UUID'v7 to strict 'Text'.
toText :: UUID -> Text
toText = T.pack . toString
{-# INLINE toText #-}

-- | Pretty-print a 'UUID'v7 to lazy 'ByteString'.
toByteString :: UUID -> ByteString
toByteString = fromString . toString
{-# INLINE toByteString #-}

-- | Parse a 'UUID'v7 from its 'String' representation.
--
-- The representation is either standard or has no dashes. Does not care about
-- the case of the letters.
parseString :: String -> Maybe UUID
parseString = parseByteString . fromString
{-# INLINE parseString #-}

-- | Parse a 'UUID'v7 from its string representation as a strict 'Text'.
--
-- The representation is either standard or has no dashes. Does not care about
-- the case of the letters.
parseText :: Text -> Maybe UUID
parseText = parseByteString . BSL.fromStrict . encodeUtf8
{-# INLINE parseText #-}

-- | Parse a 'UUID'v7 from its string representation as a lazy 'ByteString'.
--
-- The representation is either standard or has no dashes. Does not care about
-- the case of the letters.
parseByteString :: ByteString -> Maybe UUID
parseByteString bs
  | BSL.length bs == 32 = UUID <$> parse False
  | BSL.length bs == 36 = UUID <$> parse True
  | otherwise           = Nothing
  where
    parse hasDashes    = (`runGet` bs) $ runMaybeT do
      raw1 <- lift $ replicateM 4 (liftM2 (,) getWord8 getWord8)
      seg1 <- hoistMaybe $ mapM readHexPair raw1
      when hasDashes checkDash
      raw2 <- lift $ replicateM 2 (liftM2 (,) getWord8 getWord8)
      seg2 <- hoistMaybe $ mapM readHexPair raw2
      when hasDashes checkDash
      raw3 <- lift $ replicateM 2 (liftM2 (,) getWord8 getWord8)
      seg3 <- hoistMaybe $ mapM readHexPair raw3
      when hasDashes checkDash
      raw4 <- lift $ replicateM 2 (liftM2 (,) getWord8 getWord8)
      seg4 <- hoistMaybe $ mapM readHexPair raw4
      when hasDashes checkDash
      raw5 <- lift $ replicateM 6 (liftM2 (,) getWord8 getWord8)
      seg5 <- hoistMaybe $ mapM readHexPair raw5
      pure . runPut . mapM_ putWord8 $ concat [seg1, seg2, seg3, seg4, seg5]
    readHex w
      | w >= 48 && w <= 57  = Just (w - 48)
      | w >= 65 && w <= 70  = Just (w - 55)
      | w >= 97 && w <= 102 = Just (w - 87)
      | otherwise           = Nothing
    readHexPair (x, y) = do
      x' <- readHex x
      y' <- readHex y
      pure (x' * 16 + y')
    checkDash          = do
      w <- lift getWord8
      guard (w == 45)

-- | The nil UUID.
nil :: UUID
nil = UUID $ BSL.replicate 16 0
{-# INLINE nil #-}

-- | Generate a 'UUID'v7.
genUUID :: IO UUID
genUUID = head <$> genUUIDs 1
{-# INLINE genUUID #-}

-- | Generate 'n' 'UUID'v7s.
--
-- It tries its best to generate 'UUID's at the same timestamp, but it may not
-- be possible if we are asking too many 'UUID's at the same time.
--
-- It is guaranteed that the first 32768 'UUID's are generated at the same
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
-- The "NOINLINE" pragma is IMPORTANT! The logic would be flawed if '__state__'
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

-- | Get the current time in milliseconds since the Unix epoch.
getEpochMilli :: IO Word64
getEpochMilli = do
  t <- getPOSIXTime
  pure $ round $ t * 1000
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
  pure $ runGet getWord16host bs
{-# INLINE getEntropyWord16 #-}

getEntropyWord64 :: IO Word64
getEntropyWord64 = do
  bs <- BSL.fromStrict <$> getEntropy 8
  pure $ runGet getWord64host bs
{-# INLINE getEntropyWord64 #-}
