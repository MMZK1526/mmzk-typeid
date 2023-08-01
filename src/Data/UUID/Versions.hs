-- |
-- Module      : Data.TypeID.Unsafe
-- License     : MIT
-- Maintainer  : mmzk1526@outlook.com
-- Portability : GHC
--
-- Supported 'UUID' versions for 'Data.TypeID.V7.TypeID'.
--
module Data.UUID.Versions where

import           Data.UUID.Types (UUID)

-- | The supported 'UUID' versions. These constructors are used as type-level
-- tags for 'Data.TypeID.V7.TypeID''.
data UUIDVersion = V1 | V4 | V5 | V7
  deriving (Eq, Ord, Bounded, Enum)
