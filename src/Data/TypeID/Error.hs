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
import qualified Data.Text as T

-- | Errors from parsing TypeIDs.
--
-- Should NOT rely on \"grepping\" the output produced by 'show' since the
-- exact output format may differ across library versions.
data TypeIDError
  = -- | The prefix is longer than 63 characters.
    TypeIDErrorPrefixTooLong Text
    -- | The ID contains an extra underscore separator.
  | TypeIDExtraSeparator
    -- | The ID starts with an underscore separator.
  | TypeIDStartWithUnderscore Text
    -- | The ID ends with an underscore separator.
  | TypeIDEndWithUnderscore Text
    -- | The prefix contains an invalid character, namely not lowercase Latin.
  | TypeIDErrorPrefixInvalidChar Text Char
    -- | From a 'Data.KindID.V7KindID' conversion. The prefix doesn't match with
    -- the expected.
  | TypeIDErrorPrefixMismatch Text Text
    -- | The 'Data.UUID.Types.Internal.UUID' suffix has errors.
  | TypeIDErrorUUIDError
  deriving (Eq, Ord)

instance Show TypeIDError where
  show :: TypeIDError -> String
  show (TypeIDErrorPrefixTooLong txt)
    = concat [ "The prefix ", show txt
             , " with ", show (T.length txt), " characters is too long!" ]
  show TypeIDExtraSeparator
    = "The underscore separator should not be present if the prefix is empty!"
  show (TypeIDStartWithUnderscore txt)
    = concat ["The prefix ", show txt, " should not start with an underscore!"]
  show (TypeIDEndWithUnderscore txt)
    = concat ["The prefix ", show txt, " should not end with an underscore!"]
  show (TypeIDErrorPrefixInvalidChar txt c)
    = concat [ "The prefix ", show txt
             , " contains invalid character ", show c, "!"]
  show (TypeIDErrorPrefixMismatch expPrefix actPrefix)
    = concat [ "Expected prefix ", show expPrefix, " but got "
             , show actPrefix, "!" ]
  show TypeIDErrorUUIDError
    = "Invalid UUID part!"
  {-# INLINE show #-}

instance Exception TypeIDError
