# dfinity-radix-tree: A generic data integrity layer.
[![Build Status](https://travis-ci.org/dfinity-lab/hs-radix-tree.svg?branch=master)](https://travis-ci.org/dfinity-lab/hs-radix-tree)
[![Hackage](https://img.shields.io/hackage/v/dfinity-radix-tree.svg)](https://hackage.haskell.org/package/dfinity-radix-tree)
[![Dependencies](https://img.shields.io/hackage-deps/v/dfinity-radix-tree.svg)](http://packdeps.haskellers.com/feed?needle=dfinity-radix-tree)
[![License: GPLv3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

## Overview
This library allows you to construct a [Merkle tree](https://en.wikipedia.org/wiki/Merkle_tree) on top of any underlying key–value database. It works by organizing your key–value pairs into a binary [radix tree](https://en.wikipedia.org/wiki/Radix_tree), which is well suited for storing large dictionaries of fairly random keys, and is optimized for storing keys of the same length.

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
Create a `RadixTree` that is parameterized by your database. If you want to make things more explicit, then define a simple type alias and wrapper function.
```haskell
import Control.Monad.Trans.Resource (MonadResource)
import Database.LevelDB (DB, Options(..), defaultOptions)

import Network.DFINITY.RadixTree

type RadixTree' = RadixTree DB

createRadixTree'
   :: MonadResource m
   => FilePath -- Database.
   -> Maybe RadixRoot -- State root.
   -> m RadixTree'
createRadixTree' path root =
   createRadixTree bloomSize cacheSize root (path, options)
   where
   bloomSize = 262144
   cacheSize = 2048
   options   = defaultOptions {createIfMissing = True}
```
Using the definitions above, we can create a radix tree, perform some basic operations on it, and see that its contents is uniquely defined by its `RadixRoot`.
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
   liftIO $ putStrLn $ "Merkle Root: 0x" ++ pretty root
   where pretty = unpack . encode . fromShort
```

## Contribute

Feel free to join in. All are welcome. Open an [issue](https://github.com/dfinity-lab/hs-radix-tree/issues)!

## License

GPLv3
