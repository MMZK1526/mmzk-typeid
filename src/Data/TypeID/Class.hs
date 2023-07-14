-- |
-- Module      : Data.TypeID.Class
-- License     : MIT
-- Maintainer  : mmzk1526@outlook.com
-- Portability : GHC
--
-- A module with the APIs for any TypeID-ish identifier type.
--
-- These type classes are useful to define custom TypeID-ish identifier types.
-- For example, if one wishes to remove the constraints on prefix, or use a
-- different UUID version for the suffix.
--
-- It is not completed as many of the functions are still implemented
-- individually in the "Data.TypeID" and "Data.KindID" modules.
--
module Data.TypeID.Class
  (
  -- * Type classes
    IDType(..)
  , IDConv(..)
  ) where

import           Data.ByteString.Lazy (ByteString)
import           Data.Text (Text)
import           Data.TypeID.Error
import           Data.UUID.V7 (UUID)
import           Data.Word

-- | A type class for a TypeID-ish identifier type, which has a 'Text' prefix
-- and a 'UUID' suffix.
class IDType a where
  -- | Get the prefix of the identifier.
  getPrefix :: a -> Text

  -- | Get the UUID suffix of the identifier.
  getUUID :: a -> UUID

  -- | Get the timestamp of the identifier.
  getTime :: a -> Word64

-- | A type class for converting between a TypeID-ish identifier type and some
-- string representations.
class IDConv a where
  -- | Parse the identifier from its 'String' representation.
  string2ID :: String -> Either TypeIDError a

  -- | Parse the identifier from its string representation as a strict 'Text'.
  text2ID :: Text -> Either TypeIDError a

  -- | Parse the identifier from its string representation as a lazy
  -- 'ByteString'.
  byteString2ID :: ByteString -> Either TypeIDError a

  -- | Pretty-print the identifier to a 'String'.
  id2String :: a -> String

  -- | Pretty-print the identifier to a strict 'Text'.
  id2Text :: a -> Text

  -- | Pretty-print the identifier to a lazy 'ByteString'.
  id2ByteString :: a -> ByteString
