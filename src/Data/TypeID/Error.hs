-- |
-- Module      : Data.TypeID.Error
-- License     : MIT
-- Maintainer  : mmzk1526@outlook.com
-- Portability : GHC
--
-- TypeID Error type.
--
module Data.TypeID.Error
 (
  -- * Data type
  TypeIDError(..)
 ) where

import           Control.Exception
import           Data.Text (Text)

-- | Errors from parsing TypeIDs.
data TypeIDError
  = -- | The prefix longer than 63 characters.
    TypeIDErrorPrefixTooLong Int
    -- | The ID contains an extra underscore separator.
  | TypeIDExtraSeparator
    -- | The prefix contains an invalid character, namely not lowercase Latin.
  | TypeIDErrorPrefixInvalidChar Char
    -- | From a `Data.KindID.KindID` conversion. The prefix doesn't match with
    -- the expected.
  | TypeIDErrorPrefixMismatch Text Text
    -- | The 'Data.UUID.Types.Internal.UUID' suffix has errors.
  | TypeIDErrorUUIDError
  deriving (Eq, Ord)

instance Show TypeIDError where
  show :: TypeIDError -> String
  show (TypeIDErrorPrefixTooLong n)
    = concat ["Prefix with ", show n, " characters is too long!"]
  show TypeIDExtraSeparator
    = "The underscore separator should not be present if the prefix is empty!"
  show (TypeIDErrorPrefixInvalidChar c)
    = concat ["Prefix contains invalid character ", show c, "!"]
  show (TypeIDErrorPrefixMismatch expPrefix actPrefix)
    = concat [ "Expected prefix ", show expPrefix, " but got "
             , show actPrefix, "!" ]
  show TypeIDErrorUUIDError
    = "Invalid UUID part!"
  {-# INLINE show #-}

instance Exception TypeIDError
