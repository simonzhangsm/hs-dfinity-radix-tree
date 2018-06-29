{-# OPTIONS -Wall #-}

module Network.DFINITY.RadixTree.Memory
   ( loadHot
   , loadCold
   , storeHot
   , storeCold
   ) where

import Codec.Serialise (deserialise, serialise)
import Crypto.Hash.SHA256 (hash)
import Data.ByteString.Char8 as Byte (take)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.ByteString.Short (fromShort, toShort)
import Data.LruCache as LRU (insert, lookup)
import Data.Map.Strict as Map (insert, lookup)

import Network.DFINITY.RadixTree.Types

loadHot
   :: RadixDatabase config m database
   => RadixRoot
   -> RadixBuffer
   -> RadixCache
   -> database
   -> m (Maybe (RadixBranch, RadixCache))
loadHot root buffer cache database =
   case Map.lookup root buffer of
      Just branch -> pure $ Just (branch, cache)
      Nothing -> loadCold root cache database

loadCold
   :: RadixDatabase config m database
   => RadixRoot
   -> RadixCache
   -> database
   -> m (Maybe (RadixBranch, RadixCache))
loadCold root cache database =
   case LRU.lookup root cache of
      Just (branch, cache') ->
         seq cache' $ seq branch $ pure $ Just (branch, cache')
      Nothing -> do
         let key = fromShort root
         result <- load database key
         case result of
            Just bytes -> do
               let branch = deserialise $ fromStrict bytes
               let cache' = LRU.insert root branch cache
               seq cache' $ seq branch $ pure $ Just (branch, cache')
            Nothing -> pure $ Nothing

storeHot
   :: RadixRoot
   -> RadixBranch
   -> RadixBuffer
   -> RadixBuffer
storeHot = Map.insert

storeCold
   :: RadixDatabase config m database
   => RadixBranch
   -> RadixCache
   -> database
   -> m (RadixRoot, RadixCache)
storeCold branch cache database = do
   store database key bytes
   seq cache' $ pure (root, cache')
   where
   bytes = toStrict $ serialise branch
   key = Byte.take 20 $ hash bytes
   root = toShort key
   cache' = LRU.insert root branch cache
