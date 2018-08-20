# dfinity-radix-tree: A generic data integrity layer.

[![DFINITY][dfinity-shield]][dfinity]
[![Build Status][build-shield]][build]
[![Hackage][hackage-shield]][hackage]
[![Dependencies][deps-shield]][deps]
[![License: GPLv3][license-shield]][license]

## Overview

This library allows you to construct a [Merkle tree][wiki-merkle-tree] on top of
any underlying key-value database. It works by organizing your key-value pairs
into a binary [radix tree][wiki-radix-tree], which is well suited for storing
large dictionaries of fairly random keys, and is optimized for storing keys of
the same length.


## Usage

Define your database as an instance of the [`RadixDatabase`][] type class.
Instances for [LevelDB][hackage-leveldb-haskell] and [LMDB][hackage-lmdb-simple]
(experimental) are already provided.


```haskell
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Control.Monad.IO.Class (MonadIO)
import Database.LevelDB (DB, defaultReadOptions, defaultWriteOptions, get, put)

import Network.DFINITY.RadixTree

instance MonadIO m => RadixDatabase m DB where
  load database = get database defaultReadOptions
  store database = put database defaultWriteOptions
```

Create a [`RadixTree`][] that is parameterized by your database. If you want to
make things more explicit, then you can define a simple type alias and wrapper
function.

```haskell
import Control.Monad.Trans.Resource (MonadResource)
import qualified Database.LevelDB as LevelDB

import Network.DFINITY.RadixTree

type RadixTree' = RadixTree LevelDB.DB

createRadixTree'
  :: (MonadResource m)
  => FilePath        -- Database path
  -> Maybe RadixRoot -- State root
  -> m RadixTree'
createRadixTree' file root = do
  let bloomSize = 262144
  let cacheSize = 2048
  let options   = LevelDB.defaultOptions
                  { LevelDB.createIfMissing = True }
  handle <- LevelDB.open file options
  createRadixTree bloomSize cacheSize root handle
```

Using the definitions above, you can create a radix tree, perform some basic
operations on it, and see that its contents is uniquely defined by its
[`RadixRoot`][].

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.ByteString.Base16 (encode)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Short (fromShort)

import Network.DFINITY.RadixTree

main :: IO ()
main = runResourceT $ do
  tree  <- createRadixTree' "/path/to/database" Nothing
  tree' <- insertRadixTree "Hello" "World" tree
  root  <- fst <$> merkleizeRadixTree tree'

  let pretty = unpack . encode . fromShort
  liftIO (putStrLn ("State Root: 0x" ++ pretty root))
```

Running the program above should produce the following result.

```
State Root: 0x621f6e4c28b18e58d374c9236daa1a0ccba16550
```

## Contribute

Feel free to join in. All are welcome. Open an [issue][issue-tracker]!

## License

`dfinity-radix-tree` is licensed under the
[GNU General Public License version 3][license].

<!-- ----------------------------------------------------------------------- -->

[dfinity]:
    https://dfinity.org
[dfinity-shield]:
    https://img.shields.io/badge/made%20by-DFINITY-29abe2.svg
[build]:
    https://travis-ci.org/dfinity-lab/hs-dfinity-radix-tree
[build-shield]:
    https://travis-ci.org/dfinity-lab/hs-dfinity-radix-tree.svg?branch=master
[hackage]:
    https://hackage.haskell.org/package/dfinity-radix-tree
[hackage-shield]:
    https://img.shields.io/hackage/v/dfinity-radix-tree.svg
[deps]:
    https://packdeps.haskellers.com/feed?needle=dfinity-radix-tree
[deps-shield]:
    https://img.shields.io/hackage-deps/v/dfinity-radix-tree.svg
[license]:
    https://www.gnu.org/licenses/gpl-3.0
[license-shield]:
    https://img.shields.io/badge/license-GPLv3-29abe2.svg
[wiki-radix-tree]:
    https://en.wikipedia.org/wiki/Radix_tree
[wiki-merkle-tree]:
    https://en.wikipedia.org/wiki/Merkle_tree
[hackage-leveldb-haskell]:
    https://hackage.haskell.org/package/leveldb-haskell
[hackage-lmdb-simple]:
    https://hackage.haskell.org/package/lmdb-simple
[`RadixDatabase`]:
    https://hackage.haskell.org/package/dfinity-radix-tree/docs/Network-DFINITY-RadixTree.html#t:RadixDatabase
[`RadixTree`]:
    https://hackage.haskell.org/package/dfinity-radix-tree/docs/Network-DFINITY-RadixTree.html#t:RadixTree
[`RadixRoot`]:
    https://hackage.haskell.org/package/dfinity-radix-tree/docs/Network-DFINITY-RadixTree.html#t:RadixRoot
[issue-tracker]:
    https://github.com/dfinity-lab/hs-radix-tree/issues

<!-- ----------------------------------------------------------------------- -->
