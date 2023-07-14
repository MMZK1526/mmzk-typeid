module Data.TypeID.Unsafe
  (
  -- * Unsafe 'TypeID' generation
    unsafeGenTypeID
  , unsafeGenTypeIDs
  -- * Unsafe decoding
  , unsafeParseString
  , unsafeParseText
  , unsafeParseByteString
  ) where

import           Data.TypeID.Internal
