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
  -- * Unsafe decoding ('TypeID'-specific)
  , unsafeParseString
  , unsafeParseText
  , unsafeParseByteString
  -- * Unsafe decoding (class methods)
  , unsafeString2ID
  , unsafeText2ID
  , unsafeByteString2ID
  ) where

import           Data.TypeID.Class
import           Data.TypeID.Internal
