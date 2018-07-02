# dfinity-radix-tree: A generic data integrity layer.
[![Build Status](https://travis-ci.org/dfinity-lab/hs-radix-tree.svg?branch=master)](https://travis-ci.org/dfinity-lab/hs-radix-tree)
[![Hackage](https://img.shields.io/hackage/v/dfinity-radix-tree.svg)](https://hackage.haskell.org/package/dfinity-radix-tree)
[![Dependencies](https://img.shields.io/hackage-deps/v/dfinity-radix-tree.svg)](http://packdeps.haskellers.com/feed?needle=dfinity-radix-tree)
[![License: GPLv3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

## Overview
This library allows you to construct a [Merkle tree](https://en.wikipedia.org/wiki/Merkle_tree) on top of any underlying key–value database.

## Usage
Define your database as an instance of the `RadixDatabase` type class. An instance for [LevelDB](http://hackage.haskell.org/package/leveldb-haskell) is already provided.
```haskell
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Control.Monad.Trans.Resource
import Database.LevelDB
import Network.DFINITY.RadixTree

instance MonadResource m => RadixDatabase (FilePath, Options) m DB where
   create = uncurry open
   load database = get database defaultReadOptions
   store database = put database defaultWriteOptions
```
Create a `RadixTree` that is parameterized by your database. If you want to make things more explicit, then define some simple type aliases and wrapper functions.
```haskell
import Control.Monad.Trans.Resource
import Data.ByteString
import Database.LevelDB
import Network.DFINITY.RadixTree

type MerkleTree = RadixTree DB
type MerkleRoot = RadixRoot

createMerkleTree
   :: MonadResource m
   => FilePath -- Database.
   -> Maybe MerkleRoot -- State root.
   -> m MerkleTree
createMerkleTree path root =
   createRadixTree bloomSize cacheSize root (path, options)
   where
   bloomSize = 262144
   cacheSize = 2048
   options = defaultOptions {createIfMissing = True}

insertMerkleTree
   :: MonadResource m
   => ByteString -- Key.
   -> ByteString -- Value.
   -> MerkleTree -- Tree.
   -> m MerkleTree
insertMerkleTree = insertRadixTree

deleteMerkleTree
   :: MonadResource m
   => ByteString -- Key.
   -> MerkleTree -- Tree.
   -> m MerkleTree
deleteMerkleTree = deleteRadixTree

merkleizeMerkleTree
   :: MonadResource m
   => MerkleTree -- Tree.
   -> m (MerkleRoot, MerkleTree)
merkleizeMerkleTree = merkleizeRadixTree

lookupMerkleTree
   :: MonadResource m
   => ByteString -- Key.
   -> MerkleTree -- Tree.
   -> m (Maybe (ByteString, MerkleTree))
lookupMerkleTree = lookupMerkleizedRadixTree
```
Using the API above, we can perform some basic operations on the database and see that its contents is uniquely defined by its `MerkleRoot`.

## Contribute

Feel free to join in. All are welcome. Open an [issue](https://github.com/dfinity-lab/hs-radix-tree/issues)!

## License

GPLv3
