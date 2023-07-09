# mmzk-typeid

## Introduction

A [typeid](https://github.com/jetpack-io/typeid) implementation in Haskell. It is "type-safe, K-sortable, globally unique identifier" extended on top of UUIDv7.

TypeIDs are canonically encoded as lowercase strings consisting of three parts:

1. A type prefix (at most 63 characters in all lowercase ASCII [a-z]);
2. An underscore '_' separator;
3. A 128-bit UUIDv7 encoded as a 26-character string using a modified base32 encoding.

For more information, please check out the [specification](https://github.com/jetpack-io/typeid/blob/main/README.md).

## Quick start
```Haskell
{-# LANGUAGE OverloadedStrings #-}

import           Control.Exception
import           Data.TypeID (TypeID)
import qualified Data.TypeID as TID

main :: IO ()
main = do

  -- Make a typeid with prefix 'mmzk':
  typeID <- TID.genTypeID "mmzk"
  putStrLn $ TID.toString typeID

  -- Make a typeid without prefix:
  typeID <- TID.genTypeID ""
  putStrLn $ TID.toString typeID

  -- Make 10 typeids in a batch. They are guaranteed to have the same timestamp and of ascending order:
  typeIDs <- TID.genTypeIDs 10 "mmzk"
  mapM_ (putStrLn . TID.toString) typeIDs

  -- Parse a typeid from string:
  case TID.parseString "mmzk_01h455vb4pex5vsknk084sn02q" of
    Left err     -> throwIO err
    Right typeID -> TID.putStrLn $ TID.toString typeID
```

# Note
Not explicitly exported functions are considered internal and are subjected to changes.
The [Main](src/Main.hs) file serves as a temporary playground and its contents are ephemeral.

# Next up
- [x] Unit tests
- [x] Type-level typeid prefixes
- [ ] Publish a package
