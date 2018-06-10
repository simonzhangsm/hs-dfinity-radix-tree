{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS -Wall #-}

module Network.DFINITY.RadixTree
   ( RadixRoot
   , RadixTree
   , createRadixTree
   , isEmptyRadixTree
   , insertRadixTree
   , deleteRadixTree
   , merkleizeRadixTree
   , lookupMerkleizedRadixTree
   , lookupNonMerkleizedRadixTree
   , sourceRadixTree
   , sinkRadixTree
   , printMerkleizedRadixTree
   , printNonMerkleizedRadixTree
   ) where

import Codec.Serialise (deserialise)
import Control.Concurrent.Async (async, link)
import Control.Concurrent.Chan (Chan, readChan)
import Control.Concurrent.MVar (modifyMVar_, newMVar, readMVar)
import Control.Monad (forM_, forever, when)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Resource (MonadResource)
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
   let tree' = tree `bool` setRoot _radixCheckpoint tree $ flag
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
               let tree' = setCache cache' $ setRoot root' tree
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
   seq bloom $
   setBloom bloom $
   setBuffer buffer $
   setRoot root tree
   where
   prefix = createPrefix $ toBits key
   branch = setPrefix prefix $ Just value `setLeaf` def
   root = createRoot branch
   bloom = Bloom.insert root _radixBloom
   buffer = storeHot root branch _radixBuffer

-- |
-- TODO (enzo): Document behavior.
updateRadixTree
   :: RadixSearchResult -- ^ Search result.
   -> ByteString -- ^ Value.
   -> RadixTree -- ^ Radix tree.
   -> RadixTree
updateRadixTree (_ :| roots, branch :| branches, prefix :| _, _, _, cache) value tree@RadixTree {..} =
   seq bloom $
   setBloom bloom $
   setBuffer buffer $
   setCache cache $
   setRoot state tree
   where
   branch' = Just value `setLeaf` branch
   root' = createRoot branch'
   parent = listToMaybe $ zip3 roots branches [head prefix]
   bloom = flip insertList _radixBloom $ root':roots
   buffer = merkleSpoof root' parent $ storeHot root' branch' _radixBuffer
   state = bool _radixRoot root' $ null roots

-- |
-- TODO (enzo): Document behavior.
insertRadixTreeAfter
   :: RadixSearchResult -- ^ Search result.
   -> ByteString -- ^ Value.
   -> RadixTree -- ^ Radix tree.
   -> RadixTree
insertRadixTreeAfter (_ :| roots, branch :| branches, prefix :| _, _, overflow, cache) value tree@RadixTree {..} =
   seq bloom $
   setBloom bloom $
   setBuffer buffer $
   setCache cache $
   setRoot state tree
   where
   prefix' = createPrefix $ drop 1 overflow
   branch' = setPrefix prefix' $ Just value `setLeaf` def
   root' = createRoot branch'
   branch'' = test `setChild` Just root' $ branch
   root'' = createRoot branch''
   test = head overflow
   parent = listToMaybe $ zip3 roots branches [head prefix]
   bloom = flip insertList _radixBloom $ root'':root':roots
   buffer = merkleSpoof root'' parent $ storeHot root'' branch'' $ storeHot root' branch' _radixBuffer
   state = bool _radixRoot root'' $ null roots

-- |
-- TODO (enzo): Document behavior.
insertRadixTreeBefore
   :: RadixSearchResult -- ^ Search result.
   -> ByteString -- ^ Value.
   -> RadixTree -- ^ Radix tree.
   -> RadixTree
insertRadixTreeBefore (_ :| roots, branch :| branches, prefix :| _, overflow, _, cache) value tree@RadixTree {..} =
   seq bloom $
   setBloom bloom $
   setBuffer buffer $
   setCache cache $
   setRoot state tree
   where
   prefix' = createPrefix $ drop 1 overflow
   branch' = setPrefix prefix' branch
   root' = createRoot branch'
   prefix'' = createPrefix $ drop 1 prefix `bool` prefix $ null roots
   branch'' = setPrefix prefix'' $ test `setChild` Just root' $ Just value `setLeaf` def
   root'' = createRoot branch''
   test = head overflow
   parent = listToMaybe $ zip3 roots branches [head prefix]
   bloom = flip insertList _radixBloom $ root'':root':roots
   buffer = merkleSpoof root'' parent $ storeHot root'' branch'' $ storeHot root' branch' _radixBuffer
   state = bool _radixRoot root'' $ null roots

-- |
-- TODO (enzo): Document behavior.
insertRadixTreeBetween
   :: RadixSearchResult -- ^ Search result.
   -> ByteString -- ^ Value.
   -> RadixTree -- ^ Radix tree.
   -> RadixTree
insertRadixTreeBetween (_ :| roots, branch :| branches, prefix :| _, prefixOverflow, keyOverflow, cache) value tree@RadixTree {..} =
   seq bloom $
   setBloom bloom $
   setBuffer buffer $
   setCache cache $
   setRoot state tree
   where
   prefix' = createPrefix $ drop 1 keyOverflow
   branch' = setPrefix prefix' $ Just value `setLeaf` def
   root' = createRoot branch'
   prefix'' = createPrefix $ drop 1 prefixOverflow
   branch'' = setPrefix prefix'' branch
   root'' = createRoot branch''
   prefix''' = createPrefix $ drop 1 prefix `bool` prefix $ null roots
   branch''' = setPrefix prefix''' $ setChildren children def
   root''' = createRoot branch'''
   test = head keyOverflow
   children = bool id swap test (Just root', Just root'')
   parent = listToMaybe $ zip3 roots branches [head prefix]
   bloom = flip insertList _radixBloom $ root''':root'':root':roots
   buffer = merkleSpoof root''' parent $ storeHot root''' branch''' $ storeHot root'' branch'' $ storeHot root' branch' _radixBuffer
   state = bool _radixRoot root''' $ null roots

-- |
-- Delete a value from a radix tree.
deleteRadixTree
   :: MonadIO m
   => ByteString -- ^ Key.
   -> RadixTree -- ^ Radix tree.
   -> m RadixTree
deleteRadixTree key tree@RadixTree {..} =
   if isEmptyRadixTree tree
   then pure tree
   else do
      result <- searchNonMerkleizedRadixTree key tree
      case result of
         Nothing -> fail "deleteRadixTree: state root does not exist"
         Just (_ :| roots, branch :| branches, prefix :| prefixes, prefixOverflow, keyOverflow, cache) ->
            if not $ null prefixOverflow && null keyOverflow
            then pure tree
            else case getLeft branch of
               Nothing -> case getRight branch of
                  Nothing -> case zip3 roots branches $ map head $ prefix:prefixes of
                     [] -> do
                        let bloom = Bloom.insert defaultRoot _radixBloom
                        let buffer = storeHot defaultRoot def _radixBuffer
                        seq bloom $ pure $ RadixTree bloom _radixBloomBits buffer cache _radixCheckpoint _radixDatabase defaultRoot
                     (_, parentBranch, parentTest):ancestors -> do
                        if isJust $ getLeaf parentBranch
                        then do
                           let branch' = setChild parentTest Nothing parentBranch
                           let root' = createRoot branch'
                           let grandparent = listToMaybe ancestors
                           let bloom = insertList (root':drop 1 roots) _radixBloom
                           let buffer = merkleSpoof root' grandparent $ storeHot root' branch' _radixBuffer
                           let state = bool _radixRoot root' $ null ancestors
                           seq bloom $ pure $ RadixTree bloom _radixBloomBits buffer cache _radixCheckpoint _radixDatabase state
                        else case (not $ head prefix) `getChild` parentBranch of
                           Nothing -> fail "deleteRadixTree: impossible"
                           Just sibRoot -> do
                              sibResult <-
                                 case loadHot sibRoot _radixBuffer of
                                    Nothing -> loadCold sibRoot cache _radixDatabase
                                    Just sibBranch -> pure $ Just (sibBranch, cache)
                              case sibResult of
                                 Nothing -> fail "deleteRadixTree: sibling state root does not exist"
                                 Just (sibBranch, cache') -> do
                                    let bits' = head prefixes ++ (not $ head prefix):(maybe [] toBits $ getPrefix sibBranch)
                                    let prefix' = createPrefix $ drop 1 bits' `bool` bits' $ null ancestors
                                    let branch' = setPrefix prefix' sibBranch
                                    let root' = createRoot branch'
                                    let grandparent = listToMaybe ancestors
                                    let bloom = insertList (root':drop 1 roots) _radixBloom
                                    let buffer = merkleSpoof root' grandparent $ storeHot root' branch' _radixBuffer
                                    let state = bool _radixRoot root' $ null ancestors
                                    seq bloom $ pure $ RadixTree bloom _radixBloomBits buffer cache' _radixCheckpoint _radixDatabase state
                  Just right -> do
                     childResult <-
                        case loadHot right _radixBuffer of
                           Nothing -> loadCold right cache _radixDatabase
                           Just childBranch -> pure $ Just (childBranch, cache)
                     case childResult of
                        Nothing -> fail "deleteRadixTree: child state root does not exist"
                        Just (childBranch, cache') -> do
                           let bits' = prefix ++ True:(maybe [] toBits $ getPrefix childBranch)
                           let prefix' = createPrefix $ drop 1 bits' `bool` bits' $ null roots
                           let branch' = setPrefix prefix' childBranch
                           let root' = createRoot branch'
                           let parent = listToMaybe $ zip3 roots branches [head prefix]
                           let bloom = insertList (root':roots) _radixBloom
                           let buffer =  merkleSpoof root' parent $ storeHot root' branch' _radixBuffer
                           let state = bool _radixRoot root' $ null roots
                           seq bloom $ pure $ RadixTree bloom _radixBloomBits buffer cache' _radixCheckpoint _radixDatabase state
               Just left -> case getRight branch of
                  Nothing -> do
                     childResult <-
                        case loadHot left _radixBuffer of
                           Nothing -> loadCold left cache _radixDatabase
                           Just childBranch -> pure $ Just (childBranch, cache)
                     case childResult of
                        Nothing -> fail "deleteRadixTree: child state root does not exist"
                        Just (childBranch, cache') -> do
                           let bits' = prefix ++ False:(maybe [] toBits $ getPrefix childBranch)
                           let prefix' = createPrefix $ drop 1 bits' `bool` bits' $ null roots
                           let branch' = setPrefix prefix' childBranch
                           let root' = createRoot branch'
                           let parent = listToMaybe $ zip3 roots branches [head prefix]
                           let bloom = insertList (root':roots) _radixBloom
                           let buffer =  merkleSpoof root' parent $ storeHot root' branch' _radixBuffer
                           let state = bool _radixRoot root' $ null roots
                           seq bloom $ pure $ RadixTree bloom _radixBloomBits buffer cache' _radixCheckpoint _radixDatabase state
                  Just _ -> do
                     let branch' = setLeaf Nothing branch
                     let root' = createRoot branch'
                     let parent = listToMaybe $ zip3 roots branches [head prefix]
                     let bloom = insertList (root':roots) _radixBloom
                     let buffer =  merkleSpoof root' parent $ storeHot root' branch' _radixBuffer
                     let state = bool _radixRoot root' $ null roots
                     seq bloom $ pure $ RadixTree bloom _radixBloomBits buffer cache _radixCheckpoint _radixDatabase state

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
            let tree' = setCache cache' tree
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
      storeHot root $ test `setChild` Just mask $ branch

-- |
-- Merkleize a radix tree.
merkleizeRadixTree
   :: MonadIO m
   => RadixTree -- ^ Radix tree.
   -> m (RadixRoot, RadixTree)
merkleizeRadixTree RadixTree {..} = do
   (root, cache) <- loop _radixRoot _radixCache
   let tree = RadixTree bloom _radixBloomBits Map.empty cache root _radixDatabase root
   pure (root, tree)
   where
   bloom = emptyRadixBloom _radixBloomBits
   loop root cache =
      if not $ Bloom.elem root _radixBloom
      then pure (root, cache)
      else do
         result <- load
         case result of
            Nothing -> error "merkleizeRadixTree: state root does not exist"
            Just (branch@RadixBranch {..}, cache') ->
               case _radixLeft of
                  Nothing ->
                     case _radixRight of
                        Nothing -> do
                           storeCold branch cache' _radixDatabase
                        Just right -> do
                           (root', cache'') <- loop right cache'
                           let branch' = True `setChild` Just root' $ branch
                           storeCold branch' cache'' _radixDatabase
                  Just left ->
                     case _radixRight of
                        Nothing -> do
                           (root', cache'') <- loop left cache'
                           let branch' = False `setChild` Just root' $ branch
                           storeCold branch' cache'' _radixDatabase
                        Just right -> do
                           (root', cache'') <- loop left cache'
                           (root'', cache''') <- loop right cache''
                           let branch' = setChildren (Just root', Just root'') branch
                           storeCold branch' cache''' _radixDatabase
      where
      load =
         case loadHot root _radixBuffer of
            Nothing -> loadCold root cache _radixDatabase
            Just branch -> pure $ Just (branch, cache)

-- |
-- Create a conduit source from a radix tree.
sourceRadixTree
   :: MonadIO m
   => [Bool] -- ^ Bit pattern.
   -> Chan RadixRoot -- ^ Terminal state roots.
   -> Int -- ^ LRU cache size.
   -> RadixTree -- ^ Radix tree.
   -> Source m ByteString
sourceRadixTree patten chan size = \ tree -> do
   cache <- liftIO $ newMVar $ LRU.empty size
   action <- liftIO $ async $ forever $ do
      root <- readChan chan
      modifyMVar_ cache $ pure . LRU.insert root ()
   liftIO $ link action
   loop cache tree [] where
   loop cache tree@RadixTree {..} roots = do
      seen <- liftIO $ readMVar cache
      let roots' = _radixCheckpoint:roots
      if flip any roots' $ isJust . flip LRU.lookup seen
      then pure ()
      else do
         let key = fromShort _radixCheckpoint
         result <- get _radixDatabase defaultReadOptions key
         case result of
            Nothing -> fail "sourceRadixTree: state root does not exist"
            Just bytes -> do
               let RadixBranch {..} = deserialise $ fromStrict bytes
               let ok = all id $ zipWith (==) patten $ toBits $ fromShort _radixCheckpoint
               when ok $ yield bytes
               forM_ [_radixLeft, _radixRight] $ \ case
                  Nothing -> pure ()
                  Just root -> loop cache `flip` roots' $ setCheckpoint root tree

-- |
-- Create a radix tree from a conduit sink.
sinkRadixTree
   :: MonadIO m
   => Chan RadixRoot -- ^ Terminal state roots.
   -> Sink ByteString m RadixTree
sinkRadixTree = undefined

-- |
-- Print a radix tree.
printRadixTree
   :: MonadIO m
   => Bool -- ^ Overwrite state root?
   -> (RadixTree -> m (Maybe (RadixBranch, RadixCache))) -- ^ Loading strategy.
   -> RadixTree -- ^ Radix tree.
   -> m ()
printRadixTree flag load = \ tree@RadixTree {..} -> do
   let tree' = tree `bool` setRoot _radixCheckpoint tree $ flag
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
               Just root -> setRoot root tree `loop` j

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
