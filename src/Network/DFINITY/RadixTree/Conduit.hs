{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS -Wall #-}
{-# OPTIONS -Werror=incomplete-patterns #-}

-- |
-- Module     : Network.DFINITY.RadixTree.Conduit
-- Copyright  : 2018 DFINITY Stiftung
-- License    : GPL-3
-- Maintainer : Enzo Haussecker <enzo@dfinity.org>
-- Stability  : Stable
--
-- A parallel download protocol.
module Network.DFINITY.RadixTree.Conduit (

   -- ** Combinators
     sourceRadixTree
   , sinkRadixTree

   ) where

import Codec.Serialise (deserialise, deserialiseOrFail)
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.BoundedChan (BoundedChan, readChan, tryWriteChan)
import Control.Concurrent.MVar (modifyMVar_, newMVar, readMVar)
import Control.Concurrent.ReadWriteLock (RWLock)
import Control.Exception (throw)
import Control.Monad (foldM, forM_, forever, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (MonadResource, ResourceT, allocate, release)
import Crypto.Hash.SHA256 (hash)
import Data.ByteString.Lazy (fromStrict)
import Data.ByteString.Short (ShortByteString, fromShort, toShort)
import Data.ByteString.Char8 as Byte (ByteString, take)
import Data.Conduit (ConduitT, await, yield)
import Data.List as List (delete, null)
import Data.LruCache as LRU (empty, insert, lookup)
import Data.Map as Map (Map, (!), delete, empty, insert, keys, lookup, member, null, singleton)
import Data.Maybe (isJust)
import Data.Void (Void)
import Database.LevelDB (DB)

import Network.DFINITY.RadixTree.Bits
import Network.DFINITY.RadixTree.Lenses
import Network.DFINITY.RadixTree.Lock
import Network.DFINITY.RadixTree.Types

-- |
-- Create a conduit from a radix tree.
sourceRadixTree
   :: forall m database. MonadResource m
   => RadixDatabase (ConduitT () ByteString m) database
   => [Bool] -- ^ Bit mask.
   -> Int -- ^ LRU cache size in items.
   -> BoundedChan RadixRoot -- ^ Terminal state root producer.
   -> RadixTree database -- ^ Radix tree.
   -> RWLock -- ^ Database lock.
   -> ConduitT () ByteString m ()
{-# SPECIALISE sourceRadixTree
   :: [Bool]
   -> Int
   -> BoundedChan RadixRoot
   -> RadixTree DB
   -> RWLock
   -> ConduitT () ByteString (ResourceT IO) () #-}
sourceRadixTree mask cacheSize chan tree lock
   | cacheSize <= 0 = throw $ InvalidArgument "invalid LRU cache size"
   | otherwise = do
      cache <- liftIO $ newMVar $ LRU.empty cacheSize
      action <- fmap fst $ flip allocate killThread $ forkIO $ forever $ do
         root <- readChan chan
         modifyMVar_ cache $ pure . LRU.insert root ()
      loop cache tree []
      release action
      where
      loop cache subtree@RadixTree {..} accum = do
         let accum' = _radixCheckpoint:accum
         seen <- liftIO $ readMVar cache
         if flip any accum' $ isJust . flip LRU.lookup seen
         then pure ()
         else do
            let key = fromShort _radixCheckpoint
            result <- withReadLock lock $ load _radixDatabase key
            case result of
               Nothing -> pure ()
               Just bytes -> do
                  let RadixNode {..} = deserialise $ fromStrict bytes
                  let success = all id $ zipWith (==) mask $ toBits $ fromShort _radixCheckpoint
                  when success $ yield bytes
                  forM_ [_radixLeft, _radixRight] $ \ case
                     Nothing -> pure ()
                     Just root -> loop cache `flip` accum' $ setCheckpoint root subtree

-- |
-- Create a radix tree from a conduit.
sinkRadixTree
   :: forall m database. MonadResource m
   => RadixDatabase (ConduitT ByteString Void m) database
   => RadixRoot -- ^ Target state root.
   -> BoundedChan RadixRoot -- ^ Terminal state root consumer.
   -> RadixTree database -- ^ Radix tree.
   -> RWLock -- ^ Database lock.
   -> ConduitT ByteString Void m (Either [RadixRoot] (RadixTree database))
{-# SPECIALISE sinkRadixTree
   :: RadixRoot
   -> BoundedChan RadixRoot
   -> RadixTree DB
   -> RWLock
   -> ConduitT ByteString Void (ResourceT IO) (Either [RadixRoot] (RadixTree DB)) #-}
sinkRadixTree checkpoint chan tree@RadixTree {..} lock =
   loop1 Map.empty $ singleton checkpoint Nothing
   where

   -- Loop 1: The accumulation loop.
   loop1
      :: Map RadixRoot (ShortByteString, [RadixRoot])
      -> Map RadixRoot (Maybe RadixRoot)
      -> ConduitT ByteString Void m (Either [RadixRoot] (RadixTree database))
   loop1 buffer targets =
      -- Have we found all the subtrees?
      if Map.null targets
      then pure $ Right $ setCheckpoint checkpoint $ setRoot checkpoint tree
      else do
         -- Wait for a node.
         mval <- await
         case mval of
            Nothing -> pure $ Left $ keys targets
            Just bytes ->
               case deserialiseOrFail $ fromStrict bytes of
                  Left _ -> loop1 buffer targets
                  Right RadixNode {..} -> do
                     -- Does the node already exist in the database?
                     let key = Byte.take 20 $ hash bytes
                     let root = toShort key
                     let want = member root targets
                     exists <- if want
                        then pure False
                        else do
                           result <- withReadLock lock $ load _radixDatabase key
                           pure $ isJust result
                     if exists
                     then loop1 buffer $ Map.delete root targets
                     else do
                        -- Update the buffer to include the node.
                        children <- foldM step [] $ maybe id (:) _radixLeft $ maybe id (:) _radixRight []
                        let buffer' = Map.insert root (toShort bytes, children) buffer
                        -- Can we trace the node back to the target state root?
                        if want
                        then loop3 buffer' `uncurry` loop2 buffer' root (targets, [])
                        else loop1 buffer' targets
      where
      step accum root = do
         let key = fromShort root
         result <- withReadLock lock $ load _radixDatabase key
         if isJust result
         then pure accum
         else pure $ root:accum

   -- Loop 2: The aggregation loop.
   loop2
      :: Map RadixRoot (ShortByteString, [RadixRoot])
      -> RadixRoot
      -> (Map RadixRoot (Maybe RadixRoot), [(RadixRoot, ShortByteString)])
      -> (Map RadixRoot (Maybe RadixRoot), [(RadixRoot, ShortByteString)])
   loop2 buffer root accum@(targets, candidates) =
      -- Get the node from the buffer and analyze.
      case Map.lookup root buffer of
         Nothing -> accum
         Just (bytes, []) ->
            -- The node is now a candidate.
            let candidates' = (root, bytes):candidates
            in (targets, candidates')
         Just (_, children) ->
            -- The children are now targets.
            let targets' = foldr step1 targets children
            in foldl step2 (targets', candidates) children
      where
      step1 = flip Map.insert $ Just root
      step2 = flip $ loop2 buffer

   -- Loop 3: The write loop.
   loop3
      :: Map RadixRoot (ShortByteString, [RadixRoot])
      -> Map RadixRoot (Maybe RadixRoot)
      -> [(RadixRoot, ShortByteString)]
      -> ConduitT ByteString Void m (Either [RadixRoot] (RadixTree database))
   loop3 buffer targets = \ case
      [] -> loop1 buffer targets
      (root, bytes):candidates' -> do
         -- Write the node to the database.
         let key = fromShort root
         withWriteLock lock $ store _radixDatabase key $ fromShort bytes
         -- Remove all references to the node.
         let buffer' = Map.delete root buffer
         let targets' = Map.delete root targets
         -- Inspect the parent node.
         case targets ! root of
            Nothing -> loop3 buffer' targets' candidates'
            Just root' -> do
               let (bytes', siblings') = buffer ! root'
               let children' = List.delete root siblings'
               -- Has the sibling node been written to the database?
               if List.null children'
               then loop3 buffer' targets' $ (root', bytes'):candidates'
               else do
                  -- Announce a terminal state root.
                  liftIO $ void $ tryWriteChan chan root
                  -- Update the parent node.
                  let buffer'' = Map.insert root' (bytes', children') buffer'
                  loop3 buffer'' targets' candidates'
