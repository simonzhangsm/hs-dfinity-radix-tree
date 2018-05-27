{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS -Wall #-}

module Network.DFINITY.RadixTree
   ( RadixRoot
   , RadixTree
   , createRadixTree
   , isEmptyRadixTree
   , insertRadixTree
-- , deleteRadixTree
   , merkleizeRadixTree
   , lookupMerkleizedRadixTree
   , lookupNonMerkleizedRadixTree
   , printMerkleizedRadixTree
   , printNonMerkleizedRadixTree
   ) where

import Control.Arrow ((***))
import Control.Concurrent.Async (async, waitBoth)
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Resource (MonadResource)
import Data.BloomFilter as Bloom (elem, insert, insertList)
import Data.Bool (bool)
import Data.ByteString.Char8 (ByteString)
import Data.Default.Class (def)
import Data.List.NonEmpty (NonEmpty(..), fromList)
import Data.LruCache as LRU (empty)
import Data.Map.Strict as Map (empty)
import Data.Maybe (fromJust, isNothing, listToMaybe)
import Database.LevelDB (Options(..), open)
import Lens.Simple (set)

import Network.DFINITY.RadixTree.Bits
import Network.DFINITY.RadixTree.Bloom
import Network.DFINITY.RadixTree.Lenses
import Network.DFINITY.RadixTree.Memory
import Network.DFINITY.RadixTree.Types
import Network.DFINITY.RadixTree.Utilities

-- |
-- Create a radix tree.
createRadixTree
   :: MonadResource m
   => Int -- ^ Bloom filter size in bits.
   -> Int -- ^ LRU cache size in items.
   -> FilePath -- ^ LevelDB database.
   -> Maybe RadixRoot -- ^ State root.
   -> m RadixTree
createRadixTree bits size path checkpoint
   | bits <= 0 = fail "createRadixTree: invalid Bloom filter size"
   | size <= 0 = fail "createRadixTree: invalid LRU cache size"
   | otherwise = do
      database <- open path $ def {createIfMissing = True}
      (root, cache') <-
         case checkpoint of
            Nothing -> storeCold def cache database
            Just root -> do
               result <- loadCold root cache database
               case snd <$> result of
                  Nothing -> fail "createRadixTree: state root does not exist"
                  Just cache' -> pure (root, cache')
      pure $ RadixTree bloom bits Map.empty cache' root database root
   where
   bloom = emptyRadixBloom bits
   cache = LRU.empty size

-- |
-- Check if a radix tree is empty.
isEmptyRadixTree :: RadixTree -> Bool
isEmptyRadixTree = (==) defaultRoot . _radixRoot

-- |
-- Insert a key and value into a radix tree.
insertRadixTree
   :: MonadIO m
   => ByteString -- ^ Key.
   -> ByteString -- ^ Value.
   -> RadixTree -- ^ Radix tree.
   -> m RadixTree
insertRadixTree key value tree@RadixTree {..} =
   if isEmptyRadixTree tree
   then pure $ initializeRadixTree key value tree
   else searchNonMerkleizedRadixTree key tree >>= \ case
      Just result@(_, _, _, [], [], _) ->
         pure $ updateRadixTree result value tree
      Just result@(_, _, _, [], _, _) ->
         pure $ insertRadixTreeAfter result value tree
      Just result@(_, _, _, _, [], _) ->
         pure $ insertRadixTreeBefore result value tree
      Just result ->
         pure $ insertRadixTreeBetween result value tree
      Nothing -> fail "insertRadixTree: state root does not exist"

-- |
-- Search for a value in a radix tree.
searchRadixTree
   :: MonadIO m
   => Bool -- ^ Overwrite state root?
   -> (RadixTree -> m (Maybe (RadixBranch, RadixCache))) -- ^ Loading strategy.
   -> ByteString -- ^ Key.
   -> RadixTree -- ^ Radix tree.
   -> m (Maybe RadixSearchResult)
searchRadixTree flag load = \ key tree@RadixTree {..} -> do
   let key' = toBits key
   let tree' = tree `bool` set radixRoot _radixCheckpoint tree $ flag
   loop tree' Nothing [] [] [] key' where
   loop tree@RadixTree {..} implicit branches roots prefixes key = do
      -- Load the state root.
      result <- load tree
      case result of
         Nothing -> pure Nothing
         Just (branch@RadixBranch {..}, cache') -> do
            -- Calculate the prefix and overflow.
            let bits = maybe id (:) implicit $ maybe [] toBits _radixPrefix
            let prefix = matchBits bits key
            let overflow = length prefix `drop` bits
            -- Update the accumulators.
            let roots' = _radixRoot:roots
            let branches' = branch:branches
            let prefixes' = prefix:prefixes
            let key' = length prefix `drop` key
            -- Check the termination criteria.
            let bit = head key'
            let child = bool _radixLeft _radixRight bit
            if or [not $ null overflow, null key', isNothing child]
            then pure $ Just (fromList roots', fromList branches', fromList prefixes', overflow, key', cache')
            else do
               -- Recurse.
               let root' = fromJust child
               let tree' = set radixCache cache' $ set radixRoot root' tree
               let implicit' = Just bit
               loop tree' implicit' branches' roots' prefixes' key'

-- |
-- Search for a value in a Merkleized radix tree.
searchMerkleizedRadixTree
   :: MonadIO m
   => ByteString -- ^ Key.
   -> RadixTree -- ^ Radix tree.
   -> m (Maybe RadixSearchResult)
searchMerkleizedRadixTree =
   searchRadixTree True $ \ RadixTree {..} ->
      loadCold _radixRoot _radixCache _radixDatabase

-- |
-- Search for a value in a non-Merkleized radix tree.
searchNonMerkleizedRadixTree
   :: MonadIO m
   => ByteString -- ^ Key.
   -> RadixTree -- ^ Radix tree.
   -> m (Maybe RadixSearchResult)
searchNonMerkleizedRadixTree =
   searchRadixTree False $ \ RadixTree {..} ->
      case loadHot _radixRoot _radixBuffer of
         Nothing -> loadCold _radixRoot _radixCache _radixDatabase
         Just branch -> pure $ Just (branch, _radixCache)

-- |
-- TODO (enzo): Document behavior.
initializeRadixTree
   :: ByteString -- ^ Key.
   -> ByteString -- ^ Value.
   -> RadixTree -- ^ Radix tree.
   -> RadixTree
initializeRadixTree key value tree@RadixTree {..} =
   set radixBloom bloom $
   set radixBuffer buffer $
   set radixRoot root tree
   where
   bloom = insert root _radixBloom
   branch = RadixBranch prefix Nothing Nothing $ Just value
   buffer = storeHot root branch _radixBuffer
   prefix = createPrefix $ toBits key
   root = createRoot branch

-- |
-- TODO (enzo): Document behavior.
updateRadixTree
   :: RadixSearchResult -- ^ Search result.
   -> ByteString -- ^ Value.
   -> RadixTree -- ^ Radix tree.
   -> RadixTree
updateRadixTree (root :| roots, branch :| branches, prefix :| _, _, _, cache) value tree@RadixTree {..} =
   set radixBloom bloom $
   set radixBuffer buffer $
   set radixCache cache $
   set radixRoot state tree
   where
   branch' = set radixLeaf `flip` branch $ Just value
   root' = createRoot branch'
   parent = listToMaybe $ zip3 roots branches [head prefix]
   bloom = flip insertList _radixBloom $ root':roots
   buffer = merkleSpoof root' parent $ storeHot root' branch' $ freeHot root _radixBuffer
   state = bool _radixRoot root' $ null roots

-- |
-- TODO (enzo): Document behavior.
insertRadixTreeAfter
   :: RadixSearchResult -- ^ Search result.
   -> ByteString -- ^ Value.
   -> RadixTree -- ^ Radix tree.
   -> RadixTree
insertRadixTreeAfter (root :| roots, branch :| branches, prefix :| _, _, overflow, cache) value tree@RadixTree {..} =
   set radixBloom bloom $
   set radixBuffer buffer $
   set radixCache cache $
   set radixRoot state tree
   where
   prefix' = createPrefix $ drop 1 overflow
   branch' = RadixBranch prefix' Nothing Nothing $ Just value
   root' = createRoot branch'
   branch'' = Just root' `child` branch
   root'' = createRoot branch''
   child = set $ bool radixLeft radixRight $ head overflow
   parent = listToMaybe $ zip3 roots branches [head prefix]
   bloom = flip insertList _radixBloom $ root'':root':roots
   buffer = merkleSpoof root'' parent $ storeHot root'' branch'' $ storeHot root' branch' $ freeHot root _radixBuffer
   state = bool _radixRoot root'' $ null roots

-- |
-- TODO (enzo): Document behavior.
insertRadixTreeBefore
   :: RadixSearchResult -- ^ Search result.
   -> ByteString -- ^ Value.
   -> RadixTree -- ^ Radix tree.
   -> RadixTree
insertRadixTreeBefore (root :| roots, branch :| branches, prefix :| _, overflow, _, cache) value tree@RadixTree {..} =
   set radixBloom bloom $
   set radixBuffer buffer $
   set radixCache cache $
   set radixRoot state tree
   where
   prefix' = createPrefix $ drop 1 overflow
   branch' = set radixPrefix prefix' branch
   root' = createRoot branch'
   prefix'' = createPrefix $ drop 1 prefix `bool` prefix $ null roots
   branch'' = child (Just root') $ RadixBranch prefix'' Nothing Nothing $ Just value
   root'' = createRoot branch''
   child = set $ bool radixLeft radixRight $ head overflow
   parent = listToMaybe $ zip3 roots branches [head prefix]
   bloom = flip insertList _radixBloom $ root'':root':roots
   buffer = merkleSpoof root'' parent $ storeHot root'' branch'' $ storeHot root' branch' $ freeHot root _radixBuffer
   state = bool _radixRoot root'' $ null roots

-- |
-- TODO (enzo): Document behavior.
insertRadixTreeBetween
   :: RadixSearchResult -- ^ Search result.
   -> ByteString -- ^ Value.
   -> RadixTree -- ^ Radix tree.
   -> RadixTree
insertRadixTreeBetween (root :| roots, branch :| branches, prefix :| _, prefixOverflow, keyOverflow, cache) value tree@RadixTree {..} =
   set radixBloom bloom $
   set radixBuffer buffer $
   set radixCache cache $
   set radixRoot state tree
   where
   prefix' = createPrefix $ drop 1 keyOverflow
   branch' = RadixBranch prefix' Nothing Nothing $ Just value
   root' = createRoot branch'
   prefix'' = createPrefix $ drop 1 prefixOverflow
   branch'' = set radixPrefix prefix'' branch
   root'' = createRoot branch''
   prefix''' = createPrefix $ drop 1 prefix `bool` prefix $ null roots
   branch''' = RadixBranch prefix''' `uncurry` children $ Nothing
   root''' = createRoot branch'''
   children = Just *** Just $ bool (root', root'') (root'', root') $ head keyOverflow
   parent = listToMaybe $ zip3 roots branches [head prefix]
   bloom = flip insertList _radixBloom $ root''':root'':root':roots
   buffer = merkleSpoof root''' parent $ storeHot root''' branch''' $ storeHot root'' branch'' $ storeHot root' branch' $ freeHot root _radixBuffer
   state = bool _radixRoot root''' $ null roots

-- |
-- Lookup a value in a radix tree.
lookupRadixTree
   :: MonadIO m
   => (ByteString -> RadixTree -> m (Maybe RadixSearchResult)) -- ^ Search algorithm.
   -> ByteString -- ^ Key.
   -> RadixTree -- ^ Radix tree.
   -> m (Maybe (ByteString, RadixTree))
lookupRadixTree search key tree = do
   result <- search key tree
   case result of
      Nothing -> fail "lookupRadixTree: state root does not exist"
      Just (_, RadixBranch {..} :| _, _, prefixOverflow, keyOverflow, cache') ->
         if not $ null prefixOverflow && null keyOverflow
         then pure Nothing
         else pure $ do
            value <- _radixLeaf
            let tree' = set radixCache cache' tree
            pure (value, tree')

-- |
-- Lookup a value in a Merkleized radix tree.
lookupMerkleizedRadixTree
   :: MonadIO m
   => ByteString -- ^ Key.
   -> RadixTree -- ^ Radix tree.
   -> m (Maybe (ByteString, RadixTree))
lookupMerkleizedRadixTree = lookupRadixTree searchMerkleizedRadixTree

-- |
-- Lookup a value in a non-Merkleized radix tree.
lookupNonMerkleizedRadixTree
   :: MonadIO m
   => ByteString -- ^ Key.
   -> RadixTree -- ^ Radix tree.
   -> m (Maybe (ByteString, RadixTree))
lookupNonMerkleizedRadixTree = lookupRadixTree searchNonMerkleizedRadixTree

-- |
-- Mask a branch in a Merkleized radix tree.
merkleSpoof
   :: RadixRoot -- ^ State root mask.
   -> Maybe (RadixRoot, RadixBranch, Bool) -- ^ Parent.
   -> RadixBuffer -- ^ Buffer.
   -> RadixBuffer
merkleSpoof mask = \ case
   Nothing -> id
   Just (root, branch, test) ->
      let lens = set $ bool radixLeft radixRight test
      in storeHot root $ Just mask `lens` branch

-- |
-- Merkleize a radix tree.
merkleizeRadixTree
   :: MonadIO m
   => RadixTree -- ^ Radix tree.
   -> m (RadixRoot, RadixTree)
merkleizeRadixTree RadixTree {..} = do
   (root, cache) <- liftIO $ loop _radixRoot _radixCache
   let tree = RadixTree bloom _radixBloomBits Map.empty cache root _radixDatabase root
   pure (root, tree)
   where
   bloom = emptyRadixBloom _radixBloomBits
   loop root cache =
      if not $ Bloom.elem root _radixBloom
      then pure (root, cache) 
      else do
         result <- load root
         case result of
            Nothing -> error "loop: :("
            Just (branch@RadixBranch {..}, cache') ->
               case _radixLeft of
                  Nothing ->
                     case _radixRight of
                        Nothing -> do
                           storeCold branch cache' _radixDatabase
                        Just right -> do
                           (root', cache'') <- loop right cache'
                           let branch' = RadixBranch _radixPrefix Nothing (Just root') _radixLeaf
                           storeCold branch' cache'' _radixDatabase
                  Just left ->
                     case _radixRight of
                        Nothing -> do
                           (root', cache'') <- loop left cache'
                           let branch' = RadixBranch _radixPrefix (Just root') Nothing _radixLeaf
                           storeCold branch' cache'' _radixDatabase
                        Just right -> do
                           asyncLeft <- async $ loop left cache'
                           asyncRight <- async $ loop right cache'
                           ((root', _), (root'', cache''')) <- liftIO $ waitBoth asyncLeft asyncRight
                           let branch' = RadixBranch _radixPrefix (Just root') (Just root'') _radixLeaf
                           storeCold branch' cache''' _radixDatabase
      where
      load root' =
         case loadHot root' _radixBuffer of
            Nothing -> loadCold root' _radixCache _radixDatabase
            Just branch -> pure $ Just (branch, cache)

-- |
-- Print a radix tree.
printRadixTree
   :: MonadIO m
   => Bool -- ^ Overwrite state root?
   -> (RadixTree -> m (Maybe (RadixBranch, RadixCache))) -- ^ Loading strategy.
   -> RadixTree -- ^ Radix tree.
   -> m ()
printRadixTree flag load = \ tree@RadixTree {..} -> do
   let tree' = tree `bool` set radixRoot _radixCheckpoint tree $ flag
   loop tree' 0 where
   loop tree@RadixTree {..} i = do
      result <- load tree
      case fst <$> result of
         Nothing -> fail "printRadixTree: state root does not exist"
         Just branch@RadixBranch {..} -> do
            let indent = (++) $ concat $ replicate i "｜"
            liftIO $ putStrLn $ indent $ show branch
            let j = i + 1
            forM_ [_radixLeft, _radixRight] $ \ case
               Nothing -> pure ()
               Just root -> set radixRoot root tree `loop` j

-- |
-- Print a Merkleized radix tree.
printMerkleizedRadixTree
   :: MonadIO m
   => RadixTree -- ^ Radix tree.
   -> m ()
printMerkleizedRadixTree =
   printRadixTree True $ \ RadixTree {..} ->
      loadCold _radixRoot _radixCache _radixDatabase

-- |
-- Print a non-Merkleized radix tree.
printNonMerkleizedRadixTree
   :: MonadIO m
   => RadixTree -- ^ Radix tree.
   -> m ()
printNonMerkleizedRadixTree =
   printRadixTree False $ \ RadixTree {..} ->
      case loadHot _radixRoot _radixBuffer of
         Nothing -> loadCold _radixRoot _radixCache _radixDatabase
         Just branch -> pure $ Just (branch, _radixCache)
