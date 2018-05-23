{-# OPTIONS -Wall #-}

module Network.DFINITY.RadixTree.Memory
   ( loadHot
   , loadCold
   , storeHot
   , storeCold
   ) where

import Codec.Serialise (deserialise, serialise)
import Control.Monad.IO.Class (MonadIO)
import Crypto.Hash.SHA256 (hash)
import Data.ByteString.Char8 as Byte (take)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.ByteString.Short (ShortByteString, fromShort, toShort)
import Data.LruCache as LRU (insert, lookup)
import Data.Map.Strict as Map (insert, lookup)
import Database.LevelDB (defaultReadOptions, defaultWriteOptions, get, put)

import Network.DFINITY.RadixTree.Types

-- |
-- Load a branch from memory.
loadHot
   :: ShortByteString -- ^ State root.
   -> RadixBuffer -- ^ Buffer.
   -> Maybe RadixBranch
loadHot root buffer = do
   bytes <- fst <$> Map.lookup root buffer
   let branch = deserialise $ fromStrict bytes
   seq branch $ pure branch

-- |
-- Load a branch from persistent memory.
loadCold
   :: MonadIO m
   => ShortByteString -- ^ State root.
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
   :: RadixBranch -- ^ Branch.
   -> [(ShortByteString, Bool)] -- ^ Path.
   -> RadixBuffer -- ^ Buffer.
   -> (ShortByteString, RadixBuffer)
storeHot branch parents buffer =
   seq buffer' (root, buffer')
   where
   bytes = toStrict $ serialise branch
   root = toShort $ Byte.take 20 $ hash bytes
   buffer' = Map.insert root (bytes, parents) buffer

-- |
-- Store a branch in persistent memory.
storeCold
   :: MonadIO m
   => RadixBranch -- ^ Branch.
   -> RadixCache -- ^ Cache.
   -> RadixDatabase -- ^ Database.
   -> m (ShortByteString, RadixCache)
storeCold branch cache database = do
   put database defaultWriteOptions key bytes
   seq cache' $ pure (root, cache')
   where
   bytes = toStrict $ serialise branch
   key = Byte.take 20 $ hash bytes
   root = toShort key
   cache' = LRU.insert root bytes cache
