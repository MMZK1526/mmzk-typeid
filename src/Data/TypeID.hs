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
  , getPrefix
  , getUUID
  , getTime
  , Word16
  , Word64
  -- * 'TypeID' generation ('TypeID'-specific)
  , nilTypeID
  , genTypeID
  , genTypeID'
  , genTypeIDs
  , decorateTypeID
  -- * 'TypeID' generation (class methods)
  , genID
  , genID'
  , genIDs
  , decorate
  -- * Validation
  , checkPrefix
  , checkTypeID
  , checkTypeIDWithEnv
  -- * Encoding & decoding ('TypeID'-specific)
  , toString
  , toText
  , toByteString
  , parseString
  , parseText
  , parseByteString
  -- * Encoding & decoding (class methods)
  , id2String
  , id2Text
  , id2ByteString
  , string2ID
  , text2ID
  , byteString2ID
  ) where

import           Data.TypeID.Class
import           Data.TypeID.Internal
import           Data.Word
