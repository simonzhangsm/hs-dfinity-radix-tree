# dfinity-radix-tree: A generic data integrity layer.
[![DFINITY](https://img.shields.io/badge/made%20by-DFINITY-29abe2.svg)](https://dfinity.org)
[![Build Status](https://travis-ci.org/dfinity-lab/hs-radix-tree.svg?branch=master)](https://travis-ci.org/dfinity-lab/hs-radix-tree)
[![Hackage](https://img.shields.io/hackage/v/dfinity-radix-tree.svg)](https://hackage.haskell.org/package/dfinity-radix-tree)
[![Dependencies](https://img.shields.io/hackage-deps/v/dfinity-radix-tree.svg)](http://packdeps.haskellers.com/feed?needle=dfinity-radix-tree)
[![License: GPLv3](https://img.shields.io/badge/License-GPLv3-29abe2.svg)](https://www.gnu.org/licenses/gpl-3.0)

## Overview
This library allows you to construct a [Merkle tree](https://en.wikipedia.org/wiki/Merkle_tree) on top of any underlying key–value database. It works by organizing your key–value pairs into a binary [radix tree](https://en.wikipedia.org/wiki/Radix_tree), which is well suited for storing large dictionaries of fairly random keys, and is optimized for storing keys of the same length.

## Usage
Define your database as an instance of the [`RadixDatabase`](https://hackage.haskell.org/package/dfinity-radix-tree/docs/Network-DFINITY-RadixTree.html#t:RadixDatabase) type class. An instance for [LevelDB](http://hackage.haskell.org/package/leveldb-haskell) is already provided.
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
Create a [`RadixTree`](https://hackage.haskell.org/package/dfinity-radix-tree/docs/Network-DFINITY-RadixTree.html#t:RadixTree) that is parameterized by your database. If you want to make things more explicit, then you can define a simple type alias and wrapper function.
```haskell
import Control.Monad.Trans.Resource (MonadResource)
import Database.LevelDB (DB, Options(..), defaultOptions, open)

import Network.DFINITY.RadixTree

type RadixTree' = RadixTree DB

createRadixTree'
   :: MonadResource m
   => FilePath -- Database.
   -> Maybe RadixRoot -- State root.
   -> m RadixTree'
createRadixTree' file root = do
   handle <- open file options
   createRadixTree bloomSize cacheSize root handle
   where
   bloomSize = 262144
   cacheSize = 2048
   options   = defaultOptions {createIfMissing = True}
```
Using the definitions above, you can create a radix tree, perform some basic operations on it, and see that its contents is uniquely defined by its [`RadixRoot`](https://hackage.haskell.org/package/dfinity-radix-tree/docs/Network-DFINITY-RadixTree.html#t:RadixRoot).
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
   liftIO $ putStrLn $ "State Root: 0x" ++ pretty root
   where pretty = unpack . encode . fromShort
```
Running the program above should produce the following result.
```
State Root: 0x621f6e4c28b18e58d374c9236daa1a0ccba16550
```

## Contribute

Feel free to join in. All are welcome. Open an [issue](https://github.com/dfinity-lab/hs-radix-tree/issues)!

## License

GPLv3
