# mmzk-typeid

## Introduction

A [TypeID](https://github.com/jetpack-io/typeid) implementation in Haskell. It is a "type-safe, K-sortable, globally unique identifier" extended on top of UUIDv7.

TypeIDs are canonically encoded as lowercase strings consisting of three parts:

1. A type prefix (at most 63 characters in all lowercase ASCII [a-z]);
2. An underscore '_' separator;
3. A 128-bit UUIDv7 encoded as a 26-character string using a modified base32 encoding.

For more information, please check out the [specification](https://github.com/jetpack-io/typeid/blob/main/README.md).

## Highlights

In addition to the features provided by [TypeID](https://github.com/jetpack-io/typeid), this implementation also supports:

1. Generating typeids in a batch. They are guaranteed to have the same timestamp (up to the first 32768 ids) and of ascending order;
2. Encoding the prefix in the [type level](src/Data/KindID.hs), so that if you accidentally pass in a wrong prefix, the code won't compile, avoiding the need for runtime checks.

## Quick start

### Basic Usage
```Haskell
{-# LANGUAGE OverloadedStrings #-}

import           Control.Exception
import           Data.TypeID (TypeID)
import qualified Data.TypeID as TID

main :: IO ()
main = do

  -- Make a TypeID with prefix 'mmzk':
  typeID <- TID.genTypeID "mmzk"
  putStrLn $ TID.toString typeID

  -- Make a TypeID without prefix:
  typeID' <- TID.genTypeID ""
  putStrLn $ TID.toString typeID'

  -- Make 10 TypeIDs in a batch. They are guaranteed to have the same timestamp and of ascending order:
  typeIDs <- TID.genTypeIDs "mmzk" 10
  mapM_ (putStrLn . TID.toString) typeIDs

  -- Parse a TypeID from string:
  case TID.parseString "mmzk_01h455vb4pex5vsknk084sn02q" of
    Left err     -> throwIO err
    Right typeID -> TID.putStrLn $ TID.toString typeID
```

### Type-level prefix
```Haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import           Control.Exception
import           Data.KindID (KindID)
import qualified Data.KindID as KID

main :: IO ()
main = do

  -- Make a KindID with prefix 'mmzk':
  kindID <- TID.genKindID @"mmzk" -- Has type `KindID "mmzk"`
  putStrLn $ KID.toString kindID

  -- Make a KindID without prefix:
  kindID' <- KID.genKindID @"" -- Has type `KindID ""`
  putStrLn $ KID.toString kindID'

  -- Make 10 KindIDs in a batch. They are guaranteed to have the same timestamp and of ascending order:
  kindIDs <- KID.genKindIDs @"mmzk" 10
  mapM_ (putStrLn . KID.toString) kindIDs

  -- Parse a KindID from string:
  case KID.parseString "mmzk_01h455vb4pex5vsknk084sn02q" of
    Left err     -> throwIO err
    Right kindID -> KID.putStrLn $ KID.toString kindID
```

## Note
Not explicitly exported functions are considered internal and are subjected to changes.
The [Main](src/Main.hs) file serves as a temporary playground and its contents are ephemeral.

## Next up
- [x] Unit tests
- [x] Type-level TypeID prefixes
- [ ] Publish a package
