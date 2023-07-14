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
