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
  , IDType(..)
  -- * TypeID generation
  , nil
  , nilTypeID
  , genTypeID
  , genTypeIDs
  , decorate
  , decorateTypeID
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
