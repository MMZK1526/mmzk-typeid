-- |
-- Module      : Data.TypeID.Unsafe
-- License     : MIT
-- Portability : GHC
--
-- Unsafe 'Data.TypeID.V7.TypeID' functions.
--
-- It is a re-export of "Data.TypeID.V7.Unsafe".
--
module Data.TypeID.Unsafe
  (
  -- * Unsafe 'Data.TypeID.V7.TypeID' generation
    unsafeGenTypeID
  , unsafeGenTypeID'
  , unsafeGenTypeIDs
  -- * Unsafe 'Data.TypeID.V7.TypeID' generation with custom timestamp
  , unsafeGenTypeIDWithTime
  , unsafeGenTypeIDWithTime'
  , unsafeGenTypeIDsWithTime
  -- * Unsafe decoding ('Data.TypeID.V7.TypeID'-specific)
  , unsafeParseString
  , unsafeParseText
  , unsafeParseByteString
  -- * Unsafe decoding (class methods)
  , unsafeString2ID
  , unsafeText2ID
  , unsafeByteString2ID
  ) where

import           Data.TypeID.Class
import           Data.TypeID.V7.Unsafe
