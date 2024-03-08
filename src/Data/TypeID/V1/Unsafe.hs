-- |
-- Module      : Data.TypeID.V1.Unsafe
-- License     : MIT
-- Maintainer  : mmzk1526@outlook.com
-- Portability : GHC
--
-- Unsafe 'TypeIDV1' functions.
--
module Data.TypeID.V1.Unsafe
  (
  -- * Unsafe 'TypeIDV1' generation
    unsafeGenTypeID
  -- * Unsafe decoding ('TypeIDV1'-specific)
  , unsafeParseString
  , unsafeParseText
  , unsafeParseByteString
  -- * Unsafe decoding (class methods)
  , unsafeString2ID
  , unsafeText2ID
  , unsafeByteString2ID
  ) where

import           Control.Monad.IO.Class
import           Data.ByteString.Lazy (ByteString)
import           Data.Text (Text)
import           Data.TypeID.Class
import qualified Data.TypeID.Internal as TID
import           Data.TypeID.V1 (TypeIDV1)

-- | Generate a new 'TypeIDV1' from a prefix, but without checking if the prefix
-- is valid.
unsafeGenTypeID :: MonadIO m => Text -> m TypeIDV1
unsafeGenTypeID = TID.unsafeGenTypeIDV1
{-# INLINE unsafeGenTypeID #-}

-- | Parse a 'TypeIDV1' from its 'String' representation, but crashes when
-- parsing fails.
unsafeParseString :: String -> TypeIDV1
unsafeParseString = TID.unsafeParseString
{-# INLINE unsafeParseString #-}

-- | Parse a 'TypeIDV1' from its string representation as a strict 'Text', but
-- crashes when parsing fails.
unsafeParseText :: Text -> TypeIDV1
unsafeParseText = TID.unsafeParseText
{-# INLINE unsafeParseText #-}

-- | Parse a 'TypeIDV1' from its string representation as a lazy 'ByteString',
-- but crashes when parsing fails.
unsafeParseByteString :: ByteString -> TypeIDV1
unsafeParseByteString = TID.unsafeParseByteString
{-# INLINE unsafeParseByteString #-}
