{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS -Wall #-}

module Network.DFINITY.RadixTree.Memory
   ( loadHot
   , loadCold
   , storeHot
   , storeCold
   , freeHot
   ) where

import Codec.Serialise (deserialise, serialise)
import Control.Monad.IO.Class (MonadIO)
import Crypto.Hash.SHA256 (hash)
import Data.ByteString.Char8 as Byte (take)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.ByteString.Short (fromShort, toShort)
import Data.LruCache as LRU (insert, lookup)
import Data.Map.Strict as Map (delete, insert, lookup)
import Database.LevelDB (defaultReadOptions, defaultWriteOptions, get, put)

import Network.DFINITY.RadixTree.Types

-- |
-- Load a branch from memory.
loadHot
   :: RadixRoot -- ^ State root.
   -> RadixBuffer -- ^ Buffer.
   -> Maybe RadixBranch
loadHot = Map.lookup

-- |
-- Load a branch from persistent memory.
loadCold
   :: MonadIO m
   => RadixRoot -- ^ State root.
   -> RadixCache -- ^ Cache.
   -> RadixDatabase -- ^ Database.
   -> m (Maybe (RadixBranch, RadixCache))
loadCold root cache database =
   case LRU.lookup root cache of
      Just (bytes, cache') -> do
         let branch = deserialise $ fromStrict bytes
         seq branch $ pure $ Just (branch, cache')
      Nothing -> do
         let key = fromShort root
         result <- get database defaultReadOptions key
         case result of
            Just bytes -> do
               let branch = deserialise $ fromStrict bytes
               let cache' = LRU.insert root bytes cache
               seq cache' $ seq branch $ pure $ Just (branch, cache')
            Nothing -> pure $ Nothing

-- |
-- Store a branch in memory.
storeHot
   :: RadixRoot -- ^ State root.
   -> RadixBranch -- ^ Branch.
   -> RadixBuffer -- ^ Buffer.
   -> RadixBuffer
storeHot = Map.insert

-- |
-- Store a branch in persistent memory.
storeCold
   :: MonadIO m
   => RadixBranch -- ^ Branch.
   -> RadixCache -- ^ Cache.
   -> RadixDatabase -- ^ Database.
   -> m (RadixRoot, RadixCache)
storeCold branch cache database = do
   put database defaultWriteOptions key bytes
   seq cache' $ pure (root, cache')
   where
   bytes = toStrict $ serialise branch
   key = Byte.take 20 $ hash bytes
   root = toShort key
   cache' = LRU.insert root bytes cache

-- |
-- Free a branch in memory.
freeHot
   :: RadixRoot -- ^ State root.
   -> RadixBuffer -- ^ Buffer.
   -> RadixBuffer
freeHot = Map.delete
