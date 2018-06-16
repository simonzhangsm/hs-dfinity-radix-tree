{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS -Wall #-}

module Network.DFINITY.RadixTree
   ( RadixRoot
   , RadixTree
   , RadixError(..)
   , createRadixTree
   , subtreeRadixTree
   , isEmptyRadixTree
   , isValidStateRoot
   , insertRadixTree
   , deleteRadixTree
   , merkleizeRadixTree
   , lookupMerkleizedRadixTree
   , lookupNonMerkleizedRadixTree
   , sourceMerkleizedRadixTree
   , sinkMerkleizedRadixTree
   , printMerkleizedRadixTree
   , printNonMerkleizedRadixTree
   ) where

import Codec.Serialise (deserialise)
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.BoundedChan (BoundedChan, readChan)
import Control.Concurrent.MVar (modifyMVar_, newMVar, readMVar)
import Control.Exception (throw)
import Control.Monad (forM_, forever, when)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Resource (MonadResource, ResourceT, allocate, release)
import Data.BloomFilter as Bloom (elem, insert, insertList)
import Data.Bool (bool)
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Lazy (fromStrict)
import Data.ByteString.Short (fromShort)
import Data.Conduit (Sink, Source, yield)
import Data.Default.Class (def)
import Data.List.NonEmpty (NonEmpty(..), fromList)
import Data.LruCache as LRU (empty, insert, lookup)
import Data.Map.Strict as Map (empty)
import Data.Maybe (fromJust, isJust, isNothing, listToMaybe)
import Data.Tuple (swap)
import Database.LevelDB (Options(..), defaultReadOptions, get, open)

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
   -> Maybe RadixRoot -- ^ Last valid state root.
   -> m RadixTree
{-# SPECIALISE createRadixTree
   :: Int
   -> Int
   -> FilePath
   -> Maybe RadixRoot
   -> ResourceT IO RadixTree #-}
createRadixTree bloomSize cacheSize file checkpoint
   | bloomSize <= 0 = throw $ InvalidArgument "invalid Bloom filter size"
   | cacheSize <= 0 = throw $ InvalidArgument "invalid LRU cache size"
   | otherwise = do
      database <- open file $ def {createIfMissing = True}
      (root, cache') <-
         case checkpoint of
            Nothing -> storeCold def cache database
            Just root -> do
               result <- loadCold root cache database
               case snd <$> result of
                  Nothing -> throw $ StateRootDoesNotExist root
                  Just cache' -> pure (root, cache')
      pure $ RadixTree bloom bloomSize Map.empty cache' cacheSize root database root
      where
      bloom = emptyRadixBloom bloomSize
      cache = LRU.empty cacheSize

-- |
-- Create a radix subtree from a radix tree.
subtreeRadixTree
   :: MonadIO m
   => RadixRoot -- ^ State root.
   -> RadixTree -- ^ Radix tree.
   -> m RadixTree
{-# SPECIALISE subtreeRadixTree
   :: RadixRoot
   -> RadixTree
   -> ResourceT IO RadixTree #-}
subtreeRadixTree root RadixTree {..} = do
   result <- loadCold root cache _radixDatabase
   case result of
      Nothing -> throw $ StateRootDoesNotExist root
      _ -> pure $ RadixTree bloom _radixBloomSize Map.empty cache _radixCacheSize root _radixDatabase root
      where
      bloom = emptyRadixBloom _radixBloomSize
      cache = LRU.empty _radixCacheSize

-- |
-- Check if a radix tree is empty.
isEmptyRadixTree
   :: RadixTree -- ^ Radix tree.
   -> Bool
{-# INLINE isEmptyRadixTree #-}
isEmptyRadixTree = (==) defaultRoot . _radixRoot

-- |
-- Check if a state root is valid.
isValidStateRoot
   :: MonadIO m
   => RadixRoot -- ^ State root.
   -> RadixTree -- ^ Radix tree.
   -> m Bool
{-# SPECIALISE isValidStateRoot
   :: RadixRoot
   -> RadixTree
   -> ResourceT IO Bool #-}
isValidStateRoot root RadixTree {..} =
   isJust <$> get _radixDatabase defaultReadOptions key
   where
   key = fromShort root

-- |
-- Search for a value in a radix tree.
searchRadixTree
   :: MonadIO m
   => Bool -- ^ Overwrite state root?
   -> (RadixTree -> m (Maybe (RadixBranch, RadixCache))) -- ^ Loading strategy.
   -> ByteString -- ^ Key.
   -> RadixTree -- ^ Radix tree.
   -> m (Either RadixError RadixSearchResult)
{-# SPECIALISE searchRadixTree
   :: Bool
   -> (RadixTree -> ResourceT IO (Maybe (RadixBranch, RadixCache)))
   -> ByteString
   -> RadixTree
   -> ResourceT IO (Either RadixError RadixSearchResult) #-}
searchRadixTree flag load = \ key tree@RadixTree {..} -> do
   let key' = toBits key
   let tree' = tree `bool` setRoot _radixCheckpoint tree $ flag
   loop Nothing [] [] [] key' tree' where
   loop implicit branches roots prefixes key tree@RadixTree {..} = do
      -- Load the root branch.
      result <- load tree
      case result of
         Nothing -> pure $ Left $ StateRootDoesNotExist _radixRoot
         Just (branch@RadixBranch {..}, cache') -> do
            -- Calculate the prefix and overflow.
            let bits = maybe id (:) implicit $ maybe [] toBits _radixPrefix
            let prefix = matchBits bits key
            let n = length prefix
            let overflow = drop n bits
            -- Update the accumulators.
            let roots' = _radixRoot:roots
            let branches' = branch:branches
            let prefixes' = prefix:prefixes
            let key' = drop n key
            -- Check the termination criteria.
            let residue = not $ null overflow
            let bit = head key'
            let child = bool _radixLeft _radixRight bit
            if null key' || residue || isNothing child
            then pure $ Right (fromList roots', fromList branches', fromList prefixes', overflow, key', cache')
            else do
               -- Recurse.
               let root' = fromJust child
               let tree' = setCache cache' $ setRoot root' tree
               let implicit' = Just bit
               loop implicit' branches' roots' prefixes' key' tree'

-- |
-- Search for a value in a Merkleized radix tree.
searchMerkleizedRadixTree
   :: MonadIO m
   => ByteString -- ^ Key.
   -> RadixTree -- ^ Radix tree.
   -> m (Either RadixError RadixSearchResult)
{-# SPECIALISE searchMerkleizedRadixTree
   :: ByteString
   -> RadixTree
   -> ResourceT IO (Either RadixError RadixSearchResult) #-}
searchMerkleizedRadixTree =
   searchRadixTree True $ \ RadixTree {..} ->
      loadCold _radixRoot _radixCache _radixDatabase

-- |
-- Search for a value in a non-Merkleized radix tree.
searchNonMerkleizedRadixTree
   :: MonadIO m
   => ByteString -- ^ Key.
   -> RadixTree -- ^ Radix tree.
   -> m (Either RadixError RadixSearchResult)
{-# SPECIALISE searchNonMerkleizedRadixTree
   :: ByteString
   -> RadixTree
   -> ResourceT IO (Either RadixError RadixSearchResult) #-}
searchNonMerkleizedRadixTree =
   searchRadixTree False $ \ RadixTree {..} ->
      loadHot _radixRoot _radixBuffer _radixCache _radixDatabase

-- |
-- Insert a key and value into a radix tree.
insertRadixTree
   :: MonadIO m
   => ByteString -- ^ Key.
   -> ByteString -- ^ Value.
   -> RadixTree -- ^ Radix tree.
   -> m RadixTree
{-# SPECIALISE insertRadixTree
   :: ByteString
   -> ByteString
   -> RadixTree
   -> ResourceT IO RadixTree #-}
insertRadixTree key value tree =
   if isEmptyRadixTree tree
   then pure $ initializeRadixTree key value tree
   else searchNonMerkleizedRadixTree key tree >>= \ case
      Right result@(_, _, _, [], [], _) ->
         pure $ insertRadixTreeAt result value tree
      Right result@(_, _, _, [], _, _) ->
         pure $ insertRadixTreeAfter result value tree
      Right result@(_, _, _, _, [], _) ->
         pure $ insertRadixTreeBefore result value tree
      Right result ->
         pure $ insertRadixTreeBetween result value tree
      Left err -> throw err

-- TODO (enzo): Document behavior.
initializeRadixTree
   :: ByteString -- ^ Key.
   -> ByteString -- ^ Value.
   -> RadixTree -- ^ Radix tree.
   -> RadixTree
{-# INLINE initializeRadixTree #-}
initializeRadixTree key value tree@RadixTree {..} =
   seq bloom $ setBloom bloom $ setBuffer buffer $ setRoot root tree
   where
   prefix = createPrefix $ toBits key
   branch = setPrefix prefix $ Just value `setLeaf` def
   root = createRoot branch
   bloom = Bloom.insert root _radixBloom
   buffer = storeHot root branch _radixBuffer

-- TODO (enzo): Document behavior.
insertRadixTreeAt
   :: RadixSearchResult -- ^ Search result.
   -> ByteString -- ^ Value.
   -> RadixTree -- ^ Radix tree.
   -> RadixTree
{-# INLINE insertRadixTreeAt #-}
insertRadixTreeAt (_:|roots, branch:|branches, prefix:|_, _, _, cache) value tree@RadixTree {..} =
   seq bloom $ setBloom bloom $ setBuffer buffer $ setCache cache $ setRoot state tree
   where
   branch' = Just value `setLeaf` branch
   root' = createRoot branch'
   parent = listToMaybe $ zip3 roots branches prefix
   bloom = flip insertList _radixBloom $ root':roots
   buffer = merkleSpoof root' parent $ storeHot root' branch' _radixBuffer
   state = bool _radixRoot root' $ isNothing parent

-- TODO (enzo): Document behavior.
insertRadixTreeAfter
   :: RadixSearchResult -- ^ Search result.
   -> ByteString -- ^ Value.
   -> RadixTree -- ^ Radix tree.
   -> RadixTree
{-# INLINE insertRadixTreeAfter #-}
insertRadixTreeAfter (_:|roots, branch:|branches, prefix:|_, _, keyOverflow, cache) value tree@RadixTree {..} =
   seq bloom $ setBloom bloom $ setBuffer buffer $ setCache cache $ setRoot state tree
   where
   prefix' = createPrefix $ drop 1 keyOverflow
   branch' = setPrefix prefix' $ Just value `setLeaf` def
   root' = createRoot branch'
   branch'' = test `setChild` Just root' $ branch
   root'' = createRoot branch''
   test = head keyOverflow
   parent = listToMaybe $ zip3 roots branches prefix
   bloom = flip insertList _radixBloom $ root'':root':roots
   buffer = merkleSpoof root'' parent $ storeHot root'' branch'' $ storeHot root' branch' _radixBuffer
   state = bool _radixRoot root'' $ isNothing parent

-- TODO (enzo): Document behavior.
insertRadixTreeBefore
   :: RadixSearchResult -- ^ Search result.
   -> ByteString -- ^ Value.
   -> RadixTree -- ^ Radix tree.
   -> RadixTree
{-# INLINE insertRadixTreeBefore #-}
insertRadixTreeBefore (_:|roots, branch:|branches, prefix:|_, prefixOverflow, _, cache) value tree@RadixTree {..} =
   seq bloom $ setBloom bloom $ setBuffer buffer $ setCache cache $ setRoot state tree
   where
   prefix' = createPrefix $ drop 1 prefixOverflow
   branch' = setPrefix prefix' branch
   root' = createRoot branch'
   prefix'' = createPrefix $ drop 1 prefix `bool` prefix $ isNothing parent
   branch'' = setPrefix prefix'' $ test `setChild` Just root' $ Just value `setLeaf` def
   root'' = createRoot branch''
   test = head prefixOverflow
   parent = listToMaybe $ zip3 roots branches prefix
   bloom = flip insertList _radixBloom $ root'':root':roots
   buffer = merkleSpoof root'' parent $ storeHot root'' branch'' $ storeHot root' branch' _radixBuffer
   state = bool _radixRoot root'' $ isNothing parent

-- TODO (enzo): Document behavior.
insertRadixTreeBetween
   :: RadixSearchResult -- ^ Search result.
   -> ByteString -- ^ Value.
   -> RadixTree -- ^ Radix tree.
   -> RadixTree
{-# INLINE insertRadixTreeBetween #-}
insertRadixTreeBetween (_:|roots, branch:|branches, prefix:|_, prefixOverflow, keyOverflow, cache) value tree@RadixTree {..} =
   seq bloom $ setBloom bloom $ setBuffer buffer $ setCache cache $ setRoot state tree
   where
   prefix' = createPrefix $ drop 1 keyOverflow
   branch' = setPrefix prefix' $ Just value `setLeaf` def
   root' = createRoot branch'
   prefix'' = createPrefix $ drop 1 prefixOverflow
   branch'' = setPrefix prefix'' branch
   root'' = createRoot branch''
   prefix''' = createPrefix $ drop 1 prefix `bool` prefix $ isNothing parent
   branch''' = setPrefix prefix''' $ setChildren children def
   root''' = createRoot branch'''
   test = head keyOverflow
   children = bool id swap test (Just root', Just root'')
   parent = listToMaybe $ zip3 roots branches prefix
   bloom = flip insertList _radixBloom $ root''':root'':root':roots
   buffer = merkleSpoof root''' parent $ storeHot root''' branch''' $ storeHot root'' branch'' $ storeHot root' branch' _radixBuffer
   state = bool _radixRoot root''' $ isNothing parent

-- |
-- Delete a value from a radix tree.
deleteRadixTree
   :: MonadIO m
   => ByteString -- ^ Key.
   -> RadixTree -- ^ Radix tree.
   -> m RadixTree
{-# SPECIALISE deleteRadixTree
   :: ByteString
   -> RadixTree
   -> ResourceT IO RadixTree #-}
deleteRadixTree key tree@RadixTree {..} =
   if isEmptyRadixTree tree
   then pure tree
   else searchNonMerkleizedRadixTree key tree >>= \ case
      Left err -> throw err
      Right result@(_, branches, prefix:|_, [], [], cache) ->
         case branches of
            RadixBranch _ Nothing Nothing _:|[] ->
               pure $ deleteRadixTreeNoChildrenNoParent result tree
            RadixBranch _ Nothing Nothing _:|parent:_ | isJust $ getLeaf parent ->
               pure $ deleteRadixTreeNoChildrenParentWithLeaf result tree
            RadixBranch _ Nothing Nothing _:|parent:_ -> do
               let test = not $ head prefix
               let root = fromJust $ getChild test parent
               loadHot root _radixBuffer cache _radixDatabase >>= \ case
                  Nothing -> throw $ StateRootDoesNotExist root
                  Just (branch, cache') ->
                     pure $ deleteRadixTreeNoChildrenParentWithoutLeaf result branch cache' test tree
            RadixBranch _ child Nothing _:|_ | isJust child -> do
               let test = False
               let root = fromJust child
               loadHot root _radixBuffer cache _radixDatabase >>= \ case
                  Nothing -> throw $ StateRootDoesNotExist root
                  Just (branch, cache') ->
                     pure $ deleteRadixTreeOneChild result branch cache' test tree
            RadixBranch _ Nothing child _:|_ | isJust child -> do
               let test = True
               let root = fromJust child
               loadHot root _radixBuffer cache _radixDatabase >>= \ case
                  Nothing -> throw $ StateRootDoesNotExist root
                  Just (branch, cache') ->
                     pure $ deleteRadixTreeOneChild result branch cache' test tree
            _ -> pure $ deleteRadixTreeTwoChildren result tree
      Right _ -> pure tree

-- TODO (enzo): Document behavior.
deleteRadixTreeNoChildrenNoParent
   :: RadixSearchResult -- ^ Search result.
   -> RadixTree -- ^ Radix tree.
   -> RadixTree
{-# INLINE deleteRadixTreeNoChildrenNoParent #-}
deleteRadixTreeNoChildrenNoParent (_, _, _, _, _, cache) tree@RadixTree {..} =
   seq bloom $ setBloom bloom $ setBuffer buffer $ setCache cache $ setRoot state tree
   where
   bloom = Bloom.insert defaultRoot _radixBloom
   buffer = storeHot defaultRoot def _radixBuffer
   state = defaultRoot

-- TODO (enzo): Document behavior.
deleteRadixTreeNoChildrenParentWithLeaf
   :: RadixSearchResult -- ^ Search result.
   -> RadixTree -- ^ Radix tree.
   -> RadixTree
{-# INLINE deleteRadixTreeNoChildrenParentWithLeaf #-}
deleteRadixTreeNoChildrenParentWithLeaf (_:|_:roots, _:|branch:branches, prefix:|prefixes, _, _, cache) tree@RadixTree {..} =
   seq bloom $ setBloom bloom $ setBuffer buffer $ setCache cache $ setRoot state tree
   where
   branch' = setChild test Nothing branch
   root' = createRoot branch'
   test = head prefix
   parent = listToMaybe $ zip3 roots branches $ map head prefixes
   bloom = flip insertList _radixBloom $ root':roots
   buffer = merkleSpoof root' parent $ storeHot root' branch' _radixBuffer
   state = bool _radixRoot root' $ isNothing parent

-- TODO (enzo): Document behavior.
deleteRadixTreeNoChildrenParentWithoutLeaf
   :: RadixSearchResult -- ^ Search result.
   -> RadixBranch -- ^ Branch.
   -> RadixCache -- ^ Cache.
   -> Bool -- ^ Lineage.
   -> RadixTree -- ^ Radix tree.
   -> RadixTree
{-# INLINE deleteRadixTreeNoChildrenParentWithoutLeaf #-}
deleteRadixTreeNoChildrenParentWithoutLeaf (_:|_:roots, _:|_:branches, _:|prefixes, _, _, _) branch@RadixBranch {..} cache test tree@RadixTree {..} =
   seq bloom $ setBloom bloom $ setBuffer buffer $ setCache cache $ setRoot state tree
   where
   prefix' = createPrefix $ drop 1 bits `bool` bits $ isNothing parent
   branch' = setPrefix prefix' branch
   root' = createRoot branch'
   bits = head prefixes ++ test:maybe [] toBits _radixPrefix
   parent = listToMaybe $ zip3 roots branches $ map head prefixes
   bloom = flip insertList _radixBloom $ root':roots
   buffer = merkleSpoof root' parent $ storeHot root' branch' _radixBuffer
   state = bool _radixRoot root' $ isNothing parent

-- TODO (enzo): Document behavior.
deleteRadixTreeOneChild
   :: RadixSearchResult -- ^ Search result.
   -> RadixBranch -- ^ Branch.
   -> RadixCache -- ^ Cache.
   -> Bool -- ^ Lineage.
   -> RadixTree -- ^ Radix tree.
   -> RadixTree
{-# INLINE deleteRadixTreeOneChild #-}
deleteRadixTreeOneChild (_:|roots, _:|branches, prefix:|_, _, _, _) branch@RadixBranch {..} cache test tree@RadixTree {..} =
   seq bloom $ setBloom bloom $ setBuffer buffer $ setCache cache $ setRoot state tree
   where
   prefix' = createPrefix $ drop 1 bits `bool` bits $ isNothing parent
   branch' = setPrefix prefix' branch
   root' = createRoot branch'
   bits = prefix ++ test:maybe [] toBits _radixPrefix
   parent = listToMaybe $ zip3 roots branches prefix
   bloom = flip insertList _radixBloom $ root':roots
   buffer =  merkleSpoof root' parent $ storeHot root' branch' _radixBuffer
   state = bool _radixRoot root' $ isNothing parent

-- TODO (enzo): Document behavior.
deleteRadixTreeTwoChildren
   :: RadixSearchResult -- ^ Search result.
   -> RadixTree -- ^ Radix tree.
   -> RadixTree
{-# INLINE deleteRadixTreeTwoChildren #-}
deleteRadixTreeTwoChildren (_:|roots, branch:|branches, prefix:|_, _, _, cache) tree@RadixTree {..} =
   seq bloom $ setBloom bloom $ setBuffer buffer $ setCache cache $ setRoot state tree
   where
   branch' = setLeaf Nothing branch
   root' = createRoot branch'
   parent = listToMaybe $ zip3 roots branches prefix
   bloom = flip insertList _radixBloom $ root':roots
   buffer = merkleSpoof root' parent $ storeHot root' branch' _radixBuffer
   state = bool _radixRoot root' $ isNothing parent

-- |
-- Lookup a value in a radix tree.
lookupRadixTree
   :: MonadIO m
   => (ByteString -> RadixTree -> m (Either RadixError RadixSearchResult)) -- ^ Search algorithm.
   -> ByteString -- ^ Key.
   -> RadixTree -- ^ Radix tree.
   -> m (Maybe (ByteString, RadixTree))
{-# SPECIALISE lookupRadixTree
   :: (ByteString -> RadixTree -> ResourceT IO (Either RadixError RadixSearchResult))
   -> ByteString
   -> RadixTree
   -> ResourceT IO (Maybe (ByteString, RadixTree)) #-}
lookupRadixTree search key tree = do
   found <- search key tree
   case found of
      Left err -> throw err
      Right (_, RadixBranch {..}:|_, _, prefixOverflow, keyOverflow, cache') ->
         if not $ null prefixOverflow && null keyOverflow
         then pure Nothing
         else pure $ do
            value <- _radixLeaf
            let tree' = setCache cache' tree
            pure (value, tree')

-- |
-- Lookup a value in a Merkleized radix tree.
lookupMerkleizedRadixTree
   :: MonadIO m
   => ByteString -- ^ Key.
   -> RadixTree -- ^ Radix tree.
   -> m (Maybe (ByteString, RadixTree))
{-# SPECIALISE lookupMerkleizedRadixTree
   :: ByteString
   -> RadixTree
   -> ResourceT IO (Maybe (ByteString, RadixTree)) #-}
lookupMerkleizedRadixTree = lookupRadixTree searchMerkleizedRadixTree

-- |
-- Lookup a value in a non-Merkleized radix tree.
lookupNonMerkleizedRadixTree
   :: MonadIO m
   => ByteString -- ^ Key.
   -> RadixTree -- ^ Radix tree.
   -> m (Maybe (ByteString, RadixTree))
{-# SPECIALISE lookupNonMerkleizedRadixTree
   :: ByteString
   -> RadixTree
   -> ResourceT IO (Maybe (ByteString, RadixTree)) #-}
lookupNonMerkleizedRadixTree = lookupRadixTree searchNonMerkleizedRadixTree

-- |
-- Mask a branch in a Merkleized radix tree.
merkleSpoof
   :: RadixRoot -- ^ State root.
   -> Maybe (RadixRoot, RadixBranch, Bool) -- ^ Parent.
   -> RadixBuffer -- ^ Buffer.
   -> RadixBuffer
{-# INLINE merkleSpoof #-}
merkleSpoof mask = \ case
   Nothing -> id
   Just (root, branch, test) ->
      storeHot root $ test `setChild` Just mask $ branch

-- |
-- Merkleize a radix tree.
merkleizeRadixTree
   :: MonadIO m
   => RadixTree -- ^ Radix tree.
   -> m (RadixRoot, RadixTree)
{-# SPECIALISE merkleizeRadixTree
   :: RadixTree
   -> ResourceT IO (RadixRoot, RadixTree) #-}
merkleizeRadixTree RadixTree {..} = do
   (root, cache) <- loop _radixRoot _radixCache
   let tree = RadixTree bloom _radixBloomSize Map.empty cache _radixCacheSize root _radixDatabase root
   pure (root, tree)
   where
   bloom = emptyRadixBloom _radixBloomSize
   loop root cache =
      if not $ Bloom.elem root _radixBloom
      then pure (root, cache)
      else do
         -- Load the root branch.
         result <- loadHot root _radixBuffer cache _radixDatabase
         case result of
            Nothing -> throw $ StateRootDoesNotExist root
            Just (branch@RadixBranch {..}, cache') ->
               case (_radixLeft, _radixRight) of
                  -- No children.
                  (Nothing, Nothing) ->
                     storeCold branch cache' _radixDatabase
                  -- One left child.
                  (Just child, Nothing) -> do
                     (root', cache'') <- loop child cache'
                     let branch' = False `setChild` Just root' $ branch
                     storeCold branch' cache'' _radixDatabase
                  -- One right child.
                  (Nothing, Just child) -> do
                     (root', cache'') <- loop child cache'
                     let branch' = True `setChild` Just root' $ branch
                     storeCold branch' cache'' _radixDatabase
                  -- Two children.
                  (Just left, Just right) -> do
                     (root', cache'') <- loop left cache'
                     (root'', cache''') <- loop right cache''
                     let branch' = setChildren (Just root', Just root'') branch
                     storeCold branch' cache''' _radixDatabase

-- |
-- Create a conduit from a Merkleized radix tree.
sourceMerkleizedRadixTree
   :: MonadResource m
   => [Bool] -- ^ Bit patten.
   -> Int -- ^ LRU cache size in items.
   -> BoundedChan RadixRoot -- ^ Terminal state root producer.
   -> RadixTree -- ^ Radix tree.
   -> Source m ByteString
{-# SPECIALISE sourceMerkleizedRadixTree
   :: [Bool]
   -> Int
   -> BoundedChan RadixRoot
   -> RadixTree
   -> Source (ResourceT IO) ByteString #-}
sourceMerkleizedRadixTree patten cacheSize chan = \ tree -> do
   cache <- liftIO $ newMVar $ LRU.empty cacheSize
   (,) action _ <- flip allocate killThread $ forkIO $ forever $ do
      root <- readChan chan
      modifyMVar_ cache $ pure . LRU.insert root ()
   tree' <- loop cache tree []
   release action
   pure tree'
   where
   loop cache tree@RadixTree {..} roots = do
      seen <- liftIO $ readMVar cache
      let roots' = _radixCheckpoint:roots
      if flip any roots' $ isJust . flip LRU.lookup seen
      then pure ()
      else do
         let key = fromShort _radixCheckpoint
         result <- get _radixDatabase defaultReadOptions key
         case result of
            Nothing -> pure ()
            Just bytes -> do
               let RadixBranch {..} = deserialise $ fromStrict bytes
               let success = all id $ zipWith (==) patten $ toBits $ fromShort _radixCheckpoint
               when success $ yield bytes
               forM_ [_radixLeft, _radixRight] $ \ case
                  Nothing -> pure ()
                  Just root -> loop cache `flip` roots' $ setCheckpoint root tree

-- |
-- Create a Merkleized radix tree from a conduit.
sinkMerkleizedRadixTree
   :: MonadResource m
   => Int -- ^ Bloom filter size in bits.
   -> Int -- ^ LRU cache size in items.
   -> BoundedChan RadixRoot -- ^ Terminal state root consumer.
   -> FilePath -- ^ LevelDB database.
   -> RadixRoot -- ^ Target state root.
   -> Sink ByteString m (Either [RadixRoot] RadixTree)
{-# SPECIALISE sinkMerkleizedRadixTree
   :: Int
   -> Int
   -> BoundedChan RadixRoot
   -> FilePath
   -> RadixRoot
   -> Sink ByteString (ResourceT IO) (Either [RadixRoot] RadixTree) #-}
sinkMerkleizedRadixTree {- bloomSize cacheSize chan file checkpoint -} = undefined

-- |
-- Print a radix tree.
printRadixTree
   :: MonadIO m
   => Bool -- ^ Overwrite state root?
   -> (RadixTree -> m (Maybe (RadixBranch, RadixCache))) -- ^ Loading strategy.
   -> RadixTree -- ^ Radix tree.
   -> m ()
{-# SPECIALISE printRadixTree
   :: Bool
   -> (RadixTree -> ResourceT IO (Maybe (RadixBranch, RadixCache)))
   -> RadixTree
   -> ResourceT IO () #-}
printRadixTree flag load = \ tree@RadixTree {..} -> do
   let tree' = tree `bool` setRoot _radixCheckpoint tree $ flag
   loop tree' 0 where
   loop tree@RadixTree {..} i = do
      result <- load tree
      case fst <$> result of
         Nothing -> throw $ StateRootDoesNotExist _radixRoot
         Just branch@RadixBranch {..} -> do
            let indent = (++) $ concat $ replicate i "｜"
            liftIO $ putStrLn $ indent $ show branch
            let j = i + 1
            forM_ [_radixLeft, _radixRight] $ \ case
               Nothing -> pure ()
               Just root -> setRoot root tree `loop` j

-- |
-- Print a Merkleized radix tree.
printMerkleizedRadixTree
   :: MonadIO m
   => RadixTree -- ^ Radix tree.
   -> m ()
{-# SPECIALISE printMerkleizedRadixTree
   :: RadixTree
   -> ResourceT IO () #-}
printMerkleizedRadixTree =
   printRadixTree True $ \ RadixTree {..} ->
      loadCold _radixRoot _radixCache _radixDatabase

-- |
-- Print a non-Merkleized radix tree.
printNonMerkleizedRadixTree
   :: MonadIO m
   => RadixTree -- ^ Radix tree.
   -> m ()
{-# SPECIALISE printNonMerkleizedRadixTree
   :: RadixTree
   -> ResourceT IO () #-}
printNonMerkleizedRadixTree =
   printRadixTree False $ \ RadixTree {..} ->
      loadHot _radixRoot _radixBuffer _radixCache _radixDatabase
