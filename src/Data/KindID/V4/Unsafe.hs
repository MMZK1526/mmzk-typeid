-- |
-- Module      : Data.KindID.V4.Unsafe
-- License     : MIT
-- Maintainer  : mmzk1526@outlook.com
-- Portability : GHC
--
-- Unsafe 'KindIDV4' functions.
--
module Data.KindID.V4.Unsafe
  (
  -- * Unsafe 'KindIDV4' decoding ('KindIDV4'-specific)
    unsafeParseString
  , unsafeParseText
  , unsafeParseByteString
  -- * Unsafe 'KindIDV4' decoding (class methods)
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
import           Data.KindID.V4 (KindIDV4)
import           Data.TypeID.Internal (TypeID')
import           Data.TypeID.V4 (TypeIDV4)
import           Data.Text (Text)
import           Data.TypeID.Class

-- | Parse a 'KindIDV4' from its 'String' representation, but does not behave
-- correctly when parsing fails.
--
-- More specifically, if the prefix does not match, it will not complain and
-- produce the wrong 'KindIDV4'. If there are other parse errors, it will crash.
unsafeParseString :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
                  => String -> KindIDV4 prefix
unsafeParseString = KID.unsafeParseString
{-# INLINE unsafeParseString #-}

-- | Parse a 'KindIDV4' from its string representation as a strict 'Text', but
-- does not behave correctly when parsing fails.
--
-- More specifically, if the prefix does not match, it will not complain and
-- produce the wrong 'KindIDV4'. If there are other parse errors, it will crash.
unsafeParseText :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
                => Text -> KindIDV4 prefix
unsafeParseText = KID.unsafeParseText
{-# INLINE unsafeParseText #-}

-- | Parse a 'KindIDV4' from its string representation as a lazy 'ByteString',
-- but does not behave correctly when parsing fails.
--
-- More specifically, if the prefix does not match, it will not complain and
-- produce the wrong 'KindIDV4'. If there are other parse errors, it will crash.
unsafeParseByteString :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
                      => ByteString -> KindIDV4 prefix
unsafeParseByteString = KID.unsafeParseByteString
{-# INLINE unsafeParseByteString #-}

-- | Convert a 'TypeIDV4' to a 'KindIDV4'. If the actual prefix does not match
-- with the expected one as defined by the type, it does not complain and
-- produces a wrong 'KindIDV4'.
unsafeFromTypeID :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
                 => TypeIDV4 -> KindIDV4 prefix
unsafeFromTypeID = KID.unsafeFromTypeID
{-# INLINE unsafeFromTypeID #-}
