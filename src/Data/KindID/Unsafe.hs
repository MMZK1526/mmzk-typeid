-- |
-- Module      : Data.KindID.Unsafe
-- License     : MIT
-- Maintainer  : mmzk1526@outlook.com
-- Portability : GHC
--
-- Unsafe 'KindID' functions.
--
module Data.KindID.Unsafe
  (
  -- * Unsafe 'KindID' decoding ('KindID'-specific)
    unsafeParseString
  , unsafeParseText
  , unsafeParseByteString
  -- * Unsafe 'KindID' decoding (class methods)
  , unsafeString2ID
  , unsafeText2ID
  , unsafeByteString2ID
  -- * Unsafe conversion
  , unsafeFromTypeID
  ) where

import           Data.KindID.Internal
import           Data.TypeID.Class
