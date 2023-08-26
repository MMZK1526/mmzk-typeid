-- |
-- Module      : Data.TypeID.V5.Unsafe
-- License     : MIT
-- Maintainer  : mmzk1526@outlook.com
-- Portability : GHC
--
-- Unsafe 'TypeIDV5' functions.
--
module Data.TypeID.V5.Unsafe
  (
  -- * Unsafe 'TypeIDV5' generation
    unsafeGenTypeID
  -- * Unsafe decoding ('TypeIDV5'-specific)
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
import           Data.TypeID.V5 (TypeIDV5)
import           Data.UUID.Types.Internal (UUID)
import           Data.Word

-- | Generate a new 'TypeIDV5' from a prefix, but without checking if the prefix
-- is valid.
unsafeGenTypeID :: Text -> UUID -> [Word8] -> TypeIDV5
unsafeGenTypeID = TID.unsafeGenTypeIDV5
{-# INLINE unsafeGenTypeID #-}

-- | Parse a 'TypeIDV5' from its 'String' representation, but crashes when
-- parsing fails.
unsafeParseString :: String -> TypeIDV5
unsafeParseString = TID.unsafeParseString
{-# INLINE unsafeParseString #-}

-- | Parse a 'TypeIDV5' from its string representation as a strict 'Text', but
-- crashes when parsing fails.
unsafeParseText :: Text -> TypeIDV5
unsafeParseText = TID.unsafeParseText
{-# INLINE unsafeParseText #-}

-- | Parse a 'TypeIDV5' from its string representation as a lazy 'ByteString',
-- but crashes when parsing fails.
unsafeParseByteString :: ByteString -> TypeIDV5
unsafeParseByteString = TID.unsafeParseByteString
{-# INLINE unsafeParseByteString #-}
