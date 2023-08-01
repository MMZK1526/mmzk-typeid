-- |
-- Module      : Data.TypeID.V7.Unsafe
-- License     : MIT
-- Maintainer  : mmzk1526@outlook.com
-- Portability : GHC
--
-- Unsafe 'TypeID' functions.
--
module Data.TypeID.V7.Unsafe
  (
  -- * Unsafe 'TypeID' generation
    unsafeGenTypeID
  , unsafeGenTypeID'
  , unsafeGenTypeIDs
  -- * Unsafe decoding ('TypeID'-specific)
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
import           Data.TypeID.V7 (TypeID)
import           Data.UUID.Types.Internal (UUID)
import           Data.Word

-- | Generate a new 'TypeID' from a prefix, but without checking if the prefix
-- is valid.
unsafeGenTypeID :: MonadIO m => Text -> m TypeID
unsafeGenTypeID = TID.unsafeGenTypeID
{-# INLINE unsafeGenTypeID #-}

-- | Generate a new 'TypeID' from a prefix based on statelesss 'UUID'v7, but
-- without checking if the prefix is valid.
unsafeGenTypeID' :: MonadIO m => Text -> m TypeID
unsafeGenTypeID' = TID.unsafeGenTypeID'
{-# INLINE unsafeGenTypeID' #-}

-- | Generate n 'TypeID's from a prefix, but without checking if the prefix is
-- valid.
--
-- It tries its best to generate 'TypeID's at the same timestamp, but it may not
-- be possible if we are asking too many 'UUID's at the same time.
--
-- It is guaranteed that the first 32768 'TypeID's are generated at the same
-- timestamp.
unsafeGenTypeIDs :: MonadIO m => Text -> Word16 -> m [TypeID]
unsafeGenTypeIDs = TID.unsafeGenTypeIDs
{-# INLINE unsafeGenTypeIDs #-}

-- | Parse a 'TypeID' from its 'String' representation, but crashes when
-- parsing fails.
unsafeParseString :: String -> TypeID
unsafeParseString = TID.unsafeParseString
{-# INLINE unsafeParseString #-}

-- | Parse a 'TypeID' from its string representation as a strict 'Text', but
-- crashes when parsing fails.
unsafeParseText :: Text -> TypeID
unsafeParseText = TID.unsafeParseText
{-# INLINE unsafeParseText #-}

-- | Parse a 'TypeID' from its string representation as a lazy 'ByteString',
-- but crashes when parsing fails.
unsafeParseByteString :: ByteString -> TypeID
unsafeParseByteString = TID.unsafeParseByteString
{-# INLINE unsafeParseByteString #-}
