-- |
-- Module      : Data.KindID
-- License     : MIT
-- Maintainer  : mmzk1526@outlook.com
-- Portability : GHC
--
-- Similar to "Data.TypeID", but the type is statically determined in the type
-- level.
--
-- When using 'TypeID', if we want to check if the type matches, we usually need
-- to get the prefix of the 'TypeID' and compare it with the desired prefix at
-- runtime. However, with Haskell's type system, we can do this at compile time
-- instead. We call this TypeID with compile-time prefix a 'KindID'.
--
-- Of course, that would require the desired prefix to be known at compile time.
-- This is actually quite common, especially when we are using one prefix for
-- one table in the database.
--
-- For example, suppose we have a function that takes a 'KindID' with the prefix
-- "user", it may have a signature like this:
-- @ f :: KindID "user" -> IO () @
--
-- Then if we try to pass in a 'KindID' with the prefix "post", the compiler
-- will complain, thus removing the runtime check and the associated overhead.
--
-- All the prefixes are type-checked at compile time, so if we try to pass in
-- invalid prefixes, the compiler (again) will complain.
--
-- This module contains functions to generate and parse these type-level TypeIDs
-- as well as conversion functions to and from the usual term-level TypeIDs.
-- These functions are usually used with a type application, e.g.
--
-- > do
-- >   kindID <- genKindID @"user"
-- >   ...
--
module Data.KindID
  (
  -- * Data types
    KindID
  , getPrefix
  , getUUID
  , getTime
  -- * 'KindID' generation ('KindID'-specific)
  , nilKindID
  , genKindID
  , genKindID'
  , genKindIDs
  , decorateKindID
  -- * 'KindID' generation (class methods)
  , genID
  , genID'
  , genIDs
  , decorate
  -- * Validation ('KindID'-specific)
  , checkKindID
  , checkKindIDWithEnv
  -- * Validation (class methods)
  , checkID
  , checkIDWithEnv
  -- * Encoding & decoding ('KindID'-specific)
  , toString
  , toText
  , toByteString
  , parseString
  , parseText
  , parseByteString
  , parseStringM
  , parseTextM
  , parseByteStringM
  -- * Encoding & decoding (class methods)
  , id2String
  , id2Text
  , id2ByteString
  , string2ID
  , text2ID
  , byteString2ID
  , string2IDM
  , text2IDM
  , byteString2IDM
  -- * Type-level & term-level conversion
  , toTypeID
  , fromTypeID
  ) where

import           Data.KindID.Internal
import           Data.TypeID.Class
import           Data.TypeID.Internal (TypeID)
