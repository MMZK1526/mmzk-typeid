-- |
-- Module      : Data.KindID
-- License     : MIT
-- Maintainer  : mmzk1526@outlook.com
-- Portability : GHC
--
-- An implementation of the typeid specification:
-- https://github.com/jetpack-io/typeid.
--
module Data.TypeID
  (
  -- * Data types
    TypeID
  , TypeIDError(..)
  , IDType(..)
  -- * TypeID generation
  , nil
  , genTypeID
  , genTypeIDs
  , decorate
  -- * Prefix validation
  , checkPrefix
  -- * Encoding & decoding
  , toString
  , toText
  , toByteString
  , parseString
  , parseText
  , parseByteString
  , parseStringWithPrefix
  , parseTextWithPrefix
  , parseByteStringWithPrefix
  ) where

import           Data.TypeID.Class
import           Data.TypeID.Internal
