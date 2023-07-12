-- | A module with the APIs for any TypeID-ish identifier type.
--
-- It is not completed as most of the functions are still implemented
-- individually in the "Data.TypeID" and "Data.KindID" modules.
module Data.TypeID.Class where

import           Data.Text (Text)
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
