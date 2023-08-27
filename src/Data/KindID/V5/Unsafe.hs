-- |
-- Module      : Data.KindID.V5.Unsafe
-- License     : MIT
-- Maintainer  : mmzk1526@outlook.com
-- Portability : GHC
--
-- Unsafe 'KindIDV5' functions.
--
module Data.KindID.V5.Unsafe
  (
  -- * Unsafe 'KindIDV5' decoding ('KindIDV5'-specific)
    unsafeParseString
  , unsafeParseText
  , unsafeParseByteString
  -- * Unsafe 'KindIDV5' decoding (class methods)
  , unsafeString2ID
  , unsafeText2ID
  , unsafeByteString2ID
  -- * Unsafe conversion
  , unsafeFromTypeID
  ) where

import           Data.ByteString.Lazy (ByteString)
import           Data.KindID.Class
import           Data.KindID.Internal (KindID')
import qualified Data.KindID.Internal as KID
import           Data.KindID.V5 (KindIDV5)
import           Data.TypeID.Internal (TypeID')
import           Data.TypeID.V5 (TypeIDV5)
import           Data.Text (Text)
import           Data.TypeID.Class

-- | Parse a 'KindIDV5' from its 'String' representation, but does not behave
-- correctly when parsing fails.
--
-- More specifically, if the prefix does not match, it will not complain and
-- produce the wrong 'KindIDV5'. If there are other parse errors, it will crash.
unsafeParseString :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
                  => String -> KindIDV5 prefix
unsafeParseString = KID.unsafeParseString
{-# INLINE unsafeParseString #-}

-- | Parse a 'KindIDV5' from its string representation as a strict 'Text', but
-- does not behave correctly when parsing fails.
--
-- More specifically, if the prefix does not match, it will not complain and
-- produce the wrong 'KindIDV5'. If there are other parse errors, it will crash.
unsafeParseText :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
                => Text -> KindIDV5 prefix
unsafeParseText = KID.unsafeParseText
{-# INLINE unsafeParseText #-}

-- | Parse a 'KindIDV5' from its string representation as a lazy 'ByteString',
-- but does not behave correctly when parsing fails.
--
-- More specifically, if the prefix does not match, it will not complain and
-- produce the wrong 'KindIDV5'. If there are other parse errors, it will crash.
unsafeParseByteString :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
                      => ByteString -> KindIDV5 prefix
unsafeParseByteString = KID.unsafeParseByteString
{-# INLINE unsafeParseByteString #-}

-- | Convert a 'TypeIDV5' to a 'KindIDV5'. If the actual prefix does not match
-- with the expected one as defined by the type, it does not complain and
-- produces a wrong 'KindIDV5'.
unsafeFromTypeID :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
                 => TypeIDV5 -> KindIDV5 prefix
unsafeFromTypeID = KID.unsafeFromTypeID
{-# INLINE unsafeFromTypeID #-}
