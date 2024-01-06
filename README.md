# mmzk-typeid

## Introduction

A [TypeID](https://github.com/jetpack-io/typeid) implementation in Haskell. It is a "type-safe, K-sortable, globally unique identifier" extended on top of UUIDv7.

TypeIDs are canonically encoded as lowercase strings consisting of three parts:

1. A type prefix (at most 63 characters in all lowercase ASCII [a-z]);
2. An underscore '_' separator;
3. A 128-bit UUIDv7 encoded as a 26-character string using a modified base32 encoding.

For more information, please check out the [specification](https://github.com/jetpack-io/typeid/blob/main/README.md).

It also serves as a (temporary) UUIDv7 implementation in Haskell, since there are no official ones yet.

If you notice any issues or have any suggestions, please feel free to open an issue or contact me via email.

## Highlights

In addition to the features provided by [TypeID](https://github.com/jetpack-io/typeid), this implementation also supports:

1. Generating TypeIDs in a batch. They are guaranteed to have the same timestamp (up to the first 32768 ids) and of ascending order;
2. Encoding the prefix in the [type level](https://hackage.haskell.org/package/mmzk-typeid/docs/Data-KindID.html), so that if you accidentally pass in a wrong prefix, the code won't compile, avoiding the need for runtime checks.
3. Support TypeID with other UUID versions. Currently v7 (default), v1,  v4, and v5 are supported.

## Quick start

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

  -- Get components from the TypeID:
  let prefix = TID.getPrefix typeID -- "mmzk"
      uuid   = TID.getUUID typeID
      time   = TID.getTime typeID -- A 'Word64' representing the timestamp in milliseconds

  -- Make a TypeID without prefix:
  typeID' <- TID.genTypeID ""
  print typeID'

  -- Make 10 TypeIDs in a batch. They are guaranteed to have the same timestamp and of ascending order:
  typeIDs <- TID.genTypeIDs "mmzk" 10
  mapM_ print typeIDs

  -- Parse a TypeID from string:
  case TID.parseString "mmzk_01h455vb4pex5vsknk084sn02q" of
    Left err     -> throwIO err
    Right typeID -> print typeID
```

For a full list of functions on `TypeID`, see [Data.TypeID](https://hackage.haskell.org/package/mmzk-typeid/docs/Data-TypeID.html).

## More Usages

### TypeID with other UUID Versions

We also support TypeID using some other versions of `UUID`, including v1 and v4, which loses the monoticity property. To use it, simply import `Data.TypeID.V4` instead of `Data.TypeID`. The following is an example using v4:

```Haskell
{-# LANGUAGE OverloadedStrings #-}

import           Control.Exception
import           Data.TypeIDV4 (TypeIDV4)
import qualified Data.TypeIDV4 as TID

main :: IO ()
main = do

  -- Make a TypeID with prefix 'mmzk':
  typeID <- TID.genTypeID "mmzk"
  putStrLn $ TID.toString typeID

  -- Get components from the TypeID:
  let prefix = TID.getPrefix typeID -- "mmzk"
      uuid   = TID.getUUID typeID

  -- Make a TypeID without prefix:
  typeID' <- TID.genTypeID ""
  print typeID'

  -- Parse a TypeID from string:
  case TID.parseString "mmzk_5hjpeh96458fct8t49fnf9farw" of
    Left err     -> throwIO err
    Right typeID -> print typeID
```

### Type-level TypeID (KindID)
When using `TypeID`, if we want to check if the type matches, we usually need to get the prefix of the `TypeID` and compare it with the desired prefix at runtime. However, with Haskell's type system, we can do this at compile time instead. We call this TypeID with compile-time prefix a KindID.

Of course, that would require the desired prefix to be known at compile time. This is actually quite common, especially when we are using one prefix for one table in the database.

For example, suppose we have a function that takes a KindID with the prefix "user", it may have a signature like this: `f :: KindID "user" -> IO ()`.

Then if we try to pass in a KindID with the prefix "post", the compiler will complain, thus removing the runtime check and the associated overhead.

All the prefixes are type-checked at compile time, so if we try to pass in invalid prefixes, the compiler (again) will complain.

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
  kindID <- KID.genKindID @"mmzk" -- Has type `KindID "mmzk"`
  putStrLn $ KID.toString kindID

  -- Get components from the KindID:
  let prefix = KID.getPrefix kindID -- "mmzk"
      uuid   = KID.getUUID kindID
      time   = KID.getTime kindID -- A 'Word64' representing the timestamp in milliseconds

  -- Make a KindID without prefix:
  kindID' <- KID.genKindID @"" -- Has type `KindID ""`
  print kindID'

  -- Make 10 KindIDs in a batch. They are guaranteed to have the same timestamp and of ascending order:
  kindIDs <- KID.genKindIDs @"mmzk" 10
  mapM_ print kindIDs

  -- Parse a KindID from string:
  case KID.parseString @"mmzk" "mmzk_01h455vb4pex5vsknk084sn02q" of
    Left err     -> throwIO err
    Right kindID -> print kindID
```

For a full list of functions on `KindID`, see [Data.KindID](https://hackage.haskell.org/package/mmzk-typeid/docs/Data-KindID.html).

### Functions with More General Types
`TypeID` and `KindID` shares many functions with the same name and functionality. So far, we are using qualified imports to diffentiate them (*e.g* `KID.fromString` and `TID.fromString`). Alternatively, we can use the methods of `IDConv` to use the same functions for both `TypeID` and `KindID`.

```Haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplication #-}

import           Control.Exception
import           Data.KindID
import           Data.TypeID

main :: IO ()
main = do
  -- Make a TypeID with prefix 'mmzk':
  typeID <- genID @TypeID "mmzk"
  print typeID

  -- Make a KindID with prefix 'mmzk':
  kindID <- genID @(KindID "mmzk")
  print kindID

  -- Parse a TypeID from string:
  case string2ID "mmzk_01h455vb4pex5vsknk084sn02q" :: Maybe TypeID of
    Left err     -> throwIO err
    Right typeID -> print typeID

  -- Parse a KindID from string:
  case string2ID "mmzk_01h455vb4pex5vsknk084sn02q" :: Maybe (KindID "mmzk") of
    Left err     -> throwIO err
    Right kindID -> print kindID
```

We no longer need to use qualified imports, but on the down side, we need to add explicit type annotations. Therefore it is a matter of preference.

Note that with the class methods, the type application with `Symbol` no longer works as the full type must be provided. For example, `string2ID @"mmzk" "mmzk_01h455vb4pex5vsknk084sn02q"` will not compile.

For a full list of these functions, see [Data.TypeID.Class](https://hackage.haskell.org/package/mmzk-typeid/docs/Data-TypeID-Class.html).

### KindID with Data Kinds
Instead of using raw `Symbol`s as `KindID` prefixes, we can also define our custom data type for better semantics.

For example, suppose we have three tables for users, posts, and comments, and each table has a unique prefix, we can design the structure as following:

```Haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import           Data.KindID
import           Data.KindID.Class

data Prefix = User | Post | Comment

instance ToPrefix 'User where
  type PrefixSymbol 'User = "user"

instance ToPrefix 'Post where
  type PrefixSymbol 'Post = "post"

instance ToPrefix 'Comment where
  type PrefixSymbol 'Comment = "comment"
```

Now we can use `Prefix` as a prefix for `KindID`s, e.g.

```Haskell
main :: IO ()
main = do
  -- ...
  userID <- genKindID @'User -- Same as genKindID @"user"
  postID <- genKindID @'Post -- Same as genKindID @"post"
  commentID <- genKindID @'Comment -- Same as genKindID @"comment"
  -- ...
```

For more information, see [Data.KindID.Class]([src/Data/KindID/Class.hs](https://hackage.haskell.org/package/mmzk-typeid/docs/Data-KindID-Class.html)).

## Note
Not explicitly exported functions are considered internal and are subjected to changes.
