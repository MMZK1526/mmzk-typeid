-- |
-- Module      : Data.KindID.Unsafe
-- License     : MIT
-- Maintainer  : mmzk1526@outlook.com
-- Portability : GHC
--
-- Unsafe 'KindID' functions.
--
module Data.KindID.V7.Unsafe
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

import           Data.ByteString.Lazy (ByteString)
import           Data.KindID.Class
import           Data.KindID.Internal (KindID')
import qualified Data.KindID.Internal as KID
import           Data.KindID.V7 (KindID)
import           Data.TypeID.Internal (TypeID')
import           Data.TypeID.V7 (TypeID)
import           Data.Text (Text)
import           Data.TypeID.Class

-- | Parse a 'KindID' from its 'String' representation, but does not behave
-- correctly when parsing fails.
--
-- More specifically, if the prefix does not match, it will not complain and
-- produce the wrong 'KindID'. If there are other parse errors, it will crash.
unsafeParseString :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
                  => String -> KindID' version prefix
unsafeParseString = KID.unsafeParseString
{-# INLINE unsafeParseString #-}

-- | Parse a 'KindID' from its string representation as a strict 'Text', but
-- does not behave correctly when parsing fails.
--
-- More specifically, if the prefix does not match, it will not complain and
-- produce the wrong 'KindID'. If there are other parse errors, it will crash.
unsafeParseText :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
                => Text -> KindID' version prefix
unsafeParseText = KID.unsafeParseText
{-# INLINE unsafeParseText #-}

-- | Parse a 'KindID' from its string representation as a lazy 'ByteString', but
-- does not behave correctly when parsing fails.
--
-- More specifically, if the prefix does not match, it will not complain and
-- produce the wrong 'KindID'. If there are other parse errors, it will crash.
unsafeParseByteString :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
                      => ByteString -> KindID' version prefix
unsafeParseByteString = KID.unsafeParseByteString
{-# INLINE unsafeParseByteString #-}

-- | Convert a 'TypeID'' to a 'KindID''. If the actual prefix does not match
-- with the expected one as defined by the type, it does not complain and
-- produces a wrong 'KindID''.
unsafeFromTypeID :: (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
                 => TypeID' version -> KindID' version prefix
unsafeFromTypeID = KID.unsafeFromTypeID
{-# INLINE unsafeFromTypeID #-}

