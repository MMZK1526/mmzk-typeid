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
  -- * Unsafe 'KindID' decoding
    unsafeParseString
  , unsafeParseText
  , unsafeParseByteString
  -- * Unsafe conversion
  , unsafeFromTypeID
  ) where

import           Data.KindID.Internal
