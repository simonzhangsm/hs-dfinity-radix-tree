{-# OPTIONS -Wall #-}

module Network.DFINITY.RadixTree.Memory
   ( loadHot
   , loadCold
   , storeHot
   , storeCold
   ) where

import Codec.Serialise (deserialise, serialise)
import Control.Monad.Trans.Resource (ResourceT)
import Crypto.Hash.SHA256 (hash)
import Data.ByteString.Char8 as Byte (take)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.ByteString.Short (fromShort, toShort)
import Data.LruCache as LRU (insert, lookup)
import Data.Map.Strict as Map (insert, lookup)
import Database.LevelDB (DB)

import Network.DFINITY.RadixTree.Types

loadHot
   :: RadixDatabase m database
   => RadixRoot
   -> RadixBuffer
   -> RadixCache
   -> database
   -> m (Maybe (RadixNode, RadixCache))
{-# SPECIALISE loadHot
   :: RadixRoot
   -> RadixBuffer
   -> RadixCache
   -> DB
   -> ResourceT IO (Maybe (RadixNode, RadixCache)) #-}
loadHot root buffer cache database =
   case Map.lookup root buffer of
      Just node -> pure $ Just (node, cache)
      Nothing -> loadCold root cache database

loadCold
   :: RadixDatabase m database
   => RadixRoot
   -> RadixCache
   -> database
   -> m (Maybe (RadixNode, RadixCache))
{-# SPECIALISE loadCold
   :: RadixRoot
   -> RadixCache
   -> DB
   -> ResourceT IO (Maybe (RadixNode, RadixCache)) #-}
loadCold root cache database =
   case LRU.lookup root cache of
      Just (node, cache') ->
         seq cache' $ seq node $ pure $ Just (node, cache')
      Nothing -> do
         let key = fromShort root
         result <- load database key
         case result of
            Just bytes -> do
               let node = deserialise $ fromStrict bytes
               let cache' = LRU.insert root node cache
               seq cache' $ seq node $ pure $ Just (node, cache')
            Nothing -> pure $ Nothing

storeHot
   :: RadixRoot
   -> RadixNode
   -> RadixBuffer
   -> RadixBuffer
storeHot = Map.insert

storeCold
   :: RadixDatabase m database
   => RadixNode
   -> RadixCache
   -> database
   -> m (RadixRoot, RadixCache)
{-# SPECIALISE storeCold
   :: RadixNode
   -> RadixCache
   -> DB
   -> ResourceT IO (RadixRoot, RadixCache) #-}
storeCold node cache database = do
   store database key bytes
   seq cache' $ pure (root, cache')
   where
   bytes = toStrict $ serialise node
   key = Byte.take 20 $ hash bytes
   root = toShort key
   cache' = LRU.insert root node cache
