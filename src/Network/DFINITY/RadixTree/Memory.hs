{-# LANGUAGE TupleSections #-}

module Network.DFINITY.RadixTree.Memory
   ( free
   , load
   , store
   ) where

import Codec.Serialise (deserialise, serialise)
import Control.DeepSeq (force)
import Control.Monad.IO.Class (MonadIO)
import Crypto.Hash.SHA256 (hash)
import Data.ByteString.Char8 as Byte (ByteString, take)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.ByteString.Short (toShort)
import Data.LruCache as LRU (insert, lookup)
import Database.LevelDB (DB, defaultReadOptions, defaultWriteOptions, delete, get, put)

import Network.DFINITY.RadixTree.Types

free :: MonadIO m => DB -> ByteString -> m ()
free database = delete database defaultWriteOptions

load :: MonadIO m => RadixCache -> DB -> ByteString -> m (Maybe RadixBranch)
load cache database root = do
   result <- case fst <$> LRU.lookup short cache of
      Nothing -> get database defaultReadOptions root
      Just bytes -> pure $ Just bytes
   pure $ deserialise . fromStrict <$> result
   where
   short = toShort root

store :: MonadIO m => RadixCache -> DB -> RadixBranch -> m (RadixCache, ByteString)
store cache database branch = do
   put database defaultWriteOptions root bytes
   pure $ (,root) $ force $ insert short bytes cache
   where
   bytes = toStrict $ serialise branch
   root  = Byte.take 20 $ hash bytes
   short = toShort root
