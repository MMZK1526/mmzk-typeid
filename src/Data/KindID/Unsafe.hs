-- |
-- Module      : Data.KindID.Unsafe
-- License     : MIT
-- Maintainer  : mmzk1526@outlook.com
-- Portability : GHC
--
-- Unsafe 'KindID' functions.
--
-- It is a re-export of "Data.TypeID.V7.Unsafe".
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

import           Data.KindID.V7 (KindID)
import           Data.KindID.V7.Unsafe
import           Data.TypeID.Class
