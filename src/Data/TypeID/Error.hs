-- | TypeID Error type.
module Data.TypeID.Error where

import           Control.Exception
import           Data.Text (Text)

-- | Errors from parsing TypeIDs.
--
-- It will not be explicitly exported from "Data.TypeID" in the future.
data TypeIDError = TypeIDErrorPrefixTooLong Int
                 | TypeIDExtraSeparator
                 | TypeIDErrorPrefixInvalidChar Char
                 | TypeIDErrorAlreadyHasPrefix Text
                 | TypeIDErrorPrefixMismatch Text Text
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
  show (TypeIDErrorAlreadyHasPrefix prefix)
    = concat ["TypeID already has prefix ", show prefix, "!"]
  show (TypeIDErrorPrefixMismatch expPrefix actPrefix)
    = concat [ "Expected prefix ", show expPrefix, " but got "
             , show actPrefix, "!" ]
  show TypeIDErrorUUIDError
    = "Invalid UUID part!"
  {-# INLINE show #-}

instance Exception TypeIDError
