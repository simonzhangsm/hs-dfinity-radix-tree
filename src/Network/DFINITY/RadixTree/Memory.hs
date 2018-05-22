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
import Data.ByteString.Char8 as Byte (ByteString, take)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.ByteString.Short (ShortByteString, toShort)
import Data.LruCache as LRU (insert, lookup)
import Data.Map.Strict as Map (insert, lookup)
import Database.LevelDB (defaultReadOptions, defaultWriteOptions, get, put)

import Network.DFINITY.RadixTree.Types

-- |
-- Load a branch from memory.
loadHot
   :: RadixBuffer -- ^ Buffer.
   -> ByteString -- ^ State root.
   -> (Maybe RadixBranch)
loadHot buffer root =
   case Map.lookup short buffer of
      Just bytes -> do
         let branch = deserialise $ fromStrict bytes
         seq branch $ Just branch
      Nothing -> Nothing
   where
   short = toShort root

-- |
-- Load a branch from persistent memory.
loadCold
   :: MonadIO m
   => RadixCache -- ^ Cache.
   -> RadixDatabase -- ^ Database.
   -> ByteString -- ^ State root.
   -> m (Maybe (RadixBranch, RadixCache))
loadCold cache database root =
   case LRU.lookup short cache of
      Just (bytes, cache') -> do
         let branch = deserialise $ fromStrict bytes
         seq branch $ pure $ Just (branch, cache')
      Nothing -> do
         result <- get database defaultReadOptions root
         case result of
            Just bytes -> do
               let branch = deserialise $ fromStrict bytes
               let cache' = LRU.insert short bytes cache
               seq cache' $ seq branch $ pure $ Just (branch, cache')
            Nothing -> pure $ Nothing
   where
   short = toShort root

-- |
-- Store a branch in memory.
storeHot
   :: RadixBuffer -- ^ Buffer.
   -> RadixBranch -- ^ Branch.
   -> (ByteString, RadixBuffer)
storeHot buffer branch =
   seq buffer' (root, buffer')
   where
   (bytes, root, short) = unload branch
   buffer' = Map.insert short bytes buffer

-- |
-- Store a branch in persistent memory.
storeCold
   :: MonadIO m
   => RadixCache -- ^ Cache.
   -> RadixDatabase -- ^ Database.
   -> RadixBranch -- ^ Branch.
   -> m (ByteString, RadixCache)
storeCold cache database branch = do
   put database defaultWriteOptions root bytes
   seq cache' $ pure (root, cache')
   where
   (bytes, root, short) = unload branch
   cache' = LRU.insert short bytes cache

-- |
-- Unload a branch.
unload
   :: RadixBranch -- ^ Branch.
   -> (ByteString, ByteString, ShortByteString)
unload branch = (bytes, root, short)
   where
   bytes = toStrict $ serialise branch
   root = Byte.take 20 $ hash bytes
   short = toShort root
