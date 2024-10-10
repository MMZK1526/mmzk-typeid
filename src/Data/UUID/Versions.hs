-- |
-- Module      : Data.UUID.Versions
-- License     : MIT
-- Maintainer  : mmzk1526@outlook.com
-- Portability : GHC
--
-- Supported 'UUID' versions for 'Data.TypeID.TypeID''.
--
module Data.UUID.Versions
 (
  -- * Supported 'UUID' versions
    UUIDVersion(..)
  -- * Validation
  , validateWithVersion
 ) where

import           Data.Bits
import           Data.UUID.Types.Internal
import           GHC.Generics (Generic)

-- | The supported 'UUID' versions. These constructors are used as type-level
-- tags for 'Data.TypeID.TypeID''.
data UUIDVersion = V1 | V4 | V5 | V7
  deriving (Eq, Ord, Bounded, Enum, Show, Generic)

toInt :: Num a => UUIDVersion -> a
toInt V1 = 1
toInt V4 = 4
toInt V5 = 5
toInt V7 = 7
{-# INLINE toInt #-}

-- | Validate the given 'UUID' with the given 'UUIDVersion'.
--
-- The variant is supposed to be 0x2.
validateWithVersion :: UUID -> UUIDVersion -> Bool
validateWithVersion (UUID w1 w2) version
  = (w1 `shiftR` 12) .&. 0xF == toInt version && (w2 `shiftR` 62) .&. 0x3 == 0x2
{-# INLINE validateWithVersion #-}
