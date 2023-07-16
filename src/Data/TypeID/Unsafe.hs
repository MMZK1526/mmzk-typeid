-- |
-- Module      : Data.TypeID.Unsafe
-- License     : MIT
-- Maintainer  : mmzk1526@outlook.com
-- Portability : GHC
--
-- Unsafe 'TypeID' functions.
--
module Data.TypeID.Unsafe
  (
  -- * Unsafe 'TypeID' generation
    unsafeGenTypeID
  , unsafeGenTypeID'
  , unsafeGenTypeIDs
  -- * Unsafe decoding
  , unsafeParseString
  , unsafeParseText
  , unsafeParseByteString
  ) where

import           Data.TypeID.Internal
