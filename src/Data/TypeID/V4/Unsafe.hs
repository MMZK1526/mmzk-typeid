-- |
-- Module      : Data.TypeID.V4.Unsafe
-- License     : MIT
-- Maintainer  : mmzk1526@outlook.com
-- Portability : GHC
--
-- Unsafe 'TypeIDV4' functions.
--
module Data.TypeID.V4.Unsafe
  (
  -- * Unsafe 'TypeIDV4' generation
    unsafeGenTypeID
  , unsafeGenTypeID'
  , unsafeGenTypeIDs
  -- * Unsafe decoding ('TypeIDV4'-specific)
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
import           Data.TypeID.V4 (TypeIDV4)
import           Data.UUID.Types.Internal (UUID)
import           Data.Word

-- | Generate a new 'TypeIDV4' from a prefix, but without checking if the prefix
-- is valid.
unsafeGenTypeID :: MonadIO m => Text -> m TypeIDV4
unsafeGenTypeID = TID.unsafeGenTypeIDV4
{-# INLINE unsafeGenTypeID #-}

-- | Generate a new 'TypeIDV4' from a prefix using insecure 'UUID'v4.
unsafeGenTypeID' :: MonadIO m => Text -> m TypeIDV4
unsafeGenTypeID' = TID.unsafeGenTypeIDV4'
{-# INLINE unsafeGenTypeID' #-}

-- | Generate n 'TypeIDV4's from a prefix, but without checking if the prefix is
-- valid.
unsafeGenTypeIDs :: MonadIO m => Text -> Word16 -> m [TypeIDV4]
unsafeGenTypeIDs = TID.unsafeGenTypeIDV4s
{-# INLINE unsafeGenTypeIDs #-}

-- | Parse a 'TypeIDV4' from its 'String' representation, but crashes when
-- parsing fails.
unsafeParseString :: String -> TypeIDV4
unsafeParseString = TID.unsafeParseString
{-# INLINE unsafeParseString #-}

-- | Parse a 'TypeIDV4' from its string representation as a strict 'Text', but
-- crashes when parsing fails.
unsafeParseText :: Text -> TypeIDV4
unsafeParseText = TID.unsafeParseText
{-# INLINE unsafeParseText #-}

-- | Parse a 'TypeIDV4' from its string representation as a lazy 'ByteString',
-- but crashes when parsing fails.
unsafeParseByteString :: ByteString -> TypeIDV4
unsafeParseByteString = TID.unsafeParseByteString
{-# INLINE unsafeParseByteString #-}
