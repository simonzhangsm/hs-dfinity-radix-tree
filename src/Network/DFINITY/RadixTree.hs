{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS -Wall #-}
{-# OPTIONS -Werror=incomplete-patterns #-}
{-# OPTIONS -fno-warn-unused-top-binds #-}

-- |
-- Module     : Network.DFINITY.RadixTree
-- Copyright  : 2018 DFINITY Stiftung
-- License    : GPL-3
-- Maintainer : Enzo Haussecker <enzo@dfinity.org>
-- Stability  : Stable
--
-- A generic data integrity layer.
module Network.DFINITY.RadixTree (

   -- ** Class
     RadixDatabase(..)

   -- ** Types
   , RadixError(..)
   , RadixRoot
   , RadixTree

   -- ** Create
   , createRadixTree

   -- ** Insert
   , insertRadixTree

   -- ** Delete
   , deleteRadixTree

   -- ** Merkleize
   , merkleizeRadixTree

   -- ** Query
   , lookupRadixTree

   -- ** Test
   , isEmptyRadixTree
   , isValidRadixRoot

   -- ** Debug
   , contentsRadixTree
   , contentsMerkleizedRadixTree
   , contentsNonMerkleizedRadixTree

   , printRadixTree
   , printMerkleizedRadixTree
   , printNonMerkleizedRadixTree

   ) where

import Control.Exception (throw)
import Control.Monad (foldM, forM_)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Resource (ResourceT)
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.ByteString.Short (fromShort)
import Data.Default.Class (def)
import Data.List.NonEmpty (NonEmpty(..), fromList)
import Data.Maybe (fromJust, isJust, isNothing, listToMaybe)
import Data.Tuple (swap)

import qualified Data.BloomFilter as Bloom
import qualified Data.LruCache    as LRU
import qualified Data.Map.Strict  as Map
import qualified Database.LevelDB as LevelDB

import Network.DFINITY.RadixTree.Bits
import Network.DFINITY.RadixTree.Bloom
import Network.DFINITY.RadixTree.Lenses
import Network.DFINITY.RadixTree.Memory
import Network.DFINITY.RadixTree.Types
import Network.DFINITY.RadixTree.Utilities

--------------------------------------------------------------------------------

-- |
-- Create a radix tree.
createRadixTree
   :: RadixDatabase m database
   => Int -- ^ Bloom filter size in bits.
   -> Int -- ^ LRU cache size in items.
   -> Maybe RadixRoot -- ^ Previous state root.
   -> database -- ^ Database.
   -> m (RadixTree database)
createRadixTree bloomSize cacheSize checkpoint database
   | bloomSize <= 0 = throw $ InvalidArgument "invalid Bloom filter size"
   | cacheSize <= 0 = throw $ InvalidArgument "invalid LRU cache size"
   | otherwise = do
      (root, cache') <-
         case checkpoint of
            Nothing -> storeCold def cache database
            Just root -> do
               result <- loadCold root cache database
               case snd <$> result of
                  Nothing -> throw $ StateRootDoesNotExist root
                  Just cache' -> pure (root, cache')
      pure $ RadixTree bloom bloomSize Map.empty cache' cacheSize root database 0 root
      where
      bloom = emptyRadixBloom bloomSize
      cache = LRU.empty cacheSize

{-# SPECIALISE createRadixTree
               :: Int
               -> Int
               -> Maybe RadixRoot
               -> LevelDB.DB
               -> ResourceT IO (RadixTree LevelDB.DB) #-}

--------------------------------------------------------------------------------

-- |
-- Check if a radix tree is empty.
isEmptyRadixTree
   :: RadixTree database -- ^ Radix tree.
   -> Bool
isEmptyRadixTree tree = _radixRoot tree == defaultRoot

{-# INLINABLE isEmptyRadixTree #-}

--------------------------------------------------------------------------------

-- |
-- Check if a state root is valid.
isValidRadixRoot
   :: RadixDatabase m database
   => RadixRoot -- ^ State root.
   -> RadixTree database -- ^ Radix tree.
   -> m Bool
isValidRadixRoot root RadixTree {..} =
   isJust <$> load _radixDatabase key
   where
   key = fromShort root

{-# SPECIALISE isValidRadixRoot
               :: RadixRoot
               -> RadixTree LevelDB.DB
               -> ResourceT IO Bool #-}

--------------------------------------------------------------------------------

-- |
-- Search for a value in a radix tree.
searchRadixTree
   :: RadixDatabase m database
   => Bool -- ^ Overwrite state root?
   -> (RadixTree database -> m (Maybe (RadixNode, RadixCache))) -- ^ Loading strategy.
   -> ByteString -- ^ Key.
   -> RadixTree database -- ^ Radix tree.
   -> m (Either RadixError RadixSearchResult)
searchRadixTree flag strategy = \ key tree@RadixTree {..} -> do
   let key' = toBits key
   let tree' = tree `bool` setRoot _radixCheckpoint tree $ flag
   loop Nothing [] [] [] key' tree' where
   loop implicit roots nodes prefixes key tree@RadixTree {..} = do
      -- Load the root node.
      result <- strategy tree
      case result of
         Nothing -> pure $ Left $ StateRootDoesNotExist _radixRoot
         Just (node@RadixNode {..}, cache') -> do
            -- Calculate the prefix and overflow.
            let bits = maybe id (:) implicit $ maybe [] toBits _radixPrefix
            let prefix = matchBits bits key
            let n = length prefix
            let overflow = drop n bits
            -- Update the accumulators.
            let roots' = _radixRoot:roots
            let nodes' = node:nodes
            let prefixes' = prefix:prefixes
            let key' = drop n key
            -- Check the termination criteria.
            let residue = not $ null overflow
            let bit = head key'
            let child = bool _radixLeft _radixRight bit
            if null key' || residue || isNothing child
            then pure $ Right (fromList roots', fromList nodes', fromList prefixes', overflow, key', cache')
            else do
               -- Recurse.
               let root' = fromJust child
               let tree' = setCache cache' $ setRoot root' tree
               let implicit' = Just bit
               loop implicit' roots' nodes' prefixes' key' tree'

{-# SPECIALISE searchRadixTree
               :: Bool
               -> (RadixTree LevelDB.DB
                   -> ResourceT IO (Maybe (RadixNode, RadixCache)))
               -> ByteString
               -> RadixTree LevelDB.DB
               -> ResourceT IO (Either RadixError RadixSearchResult) #-}

--------------------------------------------------------------------------------

-- |
-- Search for a value in a Merkleized radix tree.
searchMerkleizedRadixTree
   :: RadixDatabase m database
   => ByteString -- ^ Key.
   -> RadixTree database -- ^ Radix tree.
   -> m (Either RadixError RadixSearchResult)
searchMerkleizedRadixTree =
   searchRadixTree True $ \ RadixTree {..} ->
      loadCold _radixRoot _radixCache _radixDatabase

{-# SPECIALISE searchMerkleizedRadixTree
               :: ByteString
               -> RadixTree LevelDB.DB
               -> ResourceT IO (Either RadixError RadixSearchResult) #-}

--------------------------------------------------------------------------------

-- |
-- Search for a value in a non-Merkleized radix tree.
searchNonMerkleizedRadixTree
   :: RadixDatabase m database
   => ByteString -- ^ Key.
   -> RadixTree database -- ^ Radix tree.
   -> m (Either RadixError RadixSearchResult)
searchNonMerkleizedRadixTree =
   searchRadixTree False $ \ RadixTree {..} ->
      loadHot _radixRoot _radixBuffer _radixCache _radixDatabase

{-# SPECIALISE searchNonMerkleizedRadixTree
               :: ByteString
               -> RadixTree LevelDB.DB
               -> ResourceT IO (Either RadixError RadixSearchResult) #-}

--------------------------------------------------------------------------------

-- |
-- Insert a value into a radix tree.
insertRadixTree
   :: RadixDatabase m database
   => ByteString -- ^ Key.
   -> ByteString -- ^ Value.
   -> RadixTree database -- ^ Radix tree.
   -> m (RadixTree database)
insertRadixTree key value tree =
   if isEmptyRadixTree tree
   then pure $ initializeRadixTree key value tree
   else searchNonMerkleizedRadixTree key tree >>= \ case
      Left err -> throw err
      Right result@(_, _, _, [], [], _) ->
         pure $ insertRadixTreeAt result value tree
      Right result@(_, _, _, [], _, _) ->
         pure $ insertRadixTreeAfter result value tree
      Right result@(_, _, _, _, [], _) ->
         pure $ insertRadixTreeBefore result value tree
      Right result ->
         pure $ insertRadixTreeBetween result value tree

{-# SPECIALISE insertRadixTree
               :: ByteString
               -> ByteString
               -> RadixTree LevelDB.DB
               -> ResourceT IO (RadixTree LevelDB.DB) #-}

--------------------------------------------------------------------------------

-- TODO (enzo): Documentation.
initializeRadixTree
   :: ByteString -- ^ Key.
   -> ByteString -- ^ Value.
   -> RadixTree database -- ^ Radix tree.
   -> RadixTree database
initializeRadixTree key value tree@RadixTree {..} =
   seq bloom $ setBloom bloom $ setBuffer buffer $ setNonce nonce $ setRoot root tree
   where
   prefix = createPrefix $ toBits key
   node = setPrefix prefix $ Just value `setLeaf` def
   root = createRootFromNonce _radixNonce
   bloom = Bloom.insert root _radixBloom
   nonce = _radixNonce + 1
   buffer = storeHot root node _radixBuffer

{-# INLINABLE initializeRadixTree #-}

--------------------------------------------------------------------------------

-- TODO (enzo): Documentation.
insertRadixTreeAt
   :: RadixSearchResult -- ^ Search result.
   -> ByteString -- ^ Value.
   -> RadixTree database -- ^ Radix tree.
   -> RadixTree database
insertRadixTreeAt (_:|roots, node:|nodes, prefix:|_, _, _, cache) value tree@RadixTree {..} =
   seq bloom $ setBloom bloom $ setBuffer buffer $ setCache cache $ setNonce nonce $ setRoot state tree
   where
   node' = Just value `setLeaf` node
   root' = createRootFromNonce _radixNonce
   parent = listToMaybe $ zip3 roots nodes prefix
   bloom = flip Bloom.insertList _radixBloom $ root':roots
   buffer = merkleSpoof root' parent $ storeHot root' node' _radixBuffer
   nonce = _radixNonce + 1
   state = bool _radixRoot root' $ isNothing parent

{-# INLINABLE insertRadixTreeAt #-}

--------------------------------------------------------------------------------

-- TODO (enzo): Documentation.
insertRadixTreeAfter
   :: RadixSearchResult -- ^ Search result.
   -> ByteString -- ^ Value.
   -> RadixTree database -- ^ Radix tree.
   -> RadixTree database
insertRadixTreeAfter (_:|roots, node:|nodes, prefix:|_, _, keyOverflow, cache) value tree@RadixTree {..} =
   seq bloom $ setBloom bloom $ setBuffer buffer $ setCache cache $ setNonce nonce $ setRoot state tree
   where
   prefix' = createPrefix $ drop 1 keyOverflow
   node' = setPrefix prefix' $ Just value `setLeaf` def
   root' = createRootFromNonce _radixNonce
   node'' = test `setChild` Just root' $ node
   root'' = createRootFromNonce $ _radixNonce + 1
   test = head keyOverflow
   parent = listToMaybe $ zip3 roots nodes prefix
   bloom = flip Bloom.insertList _radixBloom $ root'':root':roots
   buffer = merkleSpoof root'' parent $ storeHot root'' node'' $ storeHot root' node' _radixBuffer
   nonce = _radixNonce + 2
   state = bool _radixRoot root'' $ isNothing parent

{-# INLINABLE insertRadixTreeAfter #-}

--------------------------------------------------------------------------------

-- TODO (enzo): Documentation.
insertRadixTreeBefore
   :: RadixSearchResult -- ^ Search result.
   -> ByteString -- ^ Value.
   -> RadixTree database -- ^ Radix tree.
   -> RadixTree database
insertRadixTreeBefore (_:|roots, node:|nodes, prefix:|_, prefixOverflow, _, cache) value tree@RadixTree {..} =
   seq bloom $ setBloom bloom $ setBuffer buffer $ setCache cache $ setNonce nonce $ setRoot state tree
   where
   prefix' = createPrefix $ drop 1 prefixOverflow
   node' = setPrefix prefix' node
   root' = createRootFromNonce _radixNonce
   prefix'' = createPrefix $ drop 1 prefix `bool` prefix $ isNothing parent
   node'' = setPrefix prefix'' $ test `setChild` Just root' $ Just value `setLeaf` def
   root'' = createRootFromNonce $ _radixNonce + 1
   test = head prefixOverflow
   parent = listToMaybe $ zip3 roots nodes prefix
   bloom = flip Bloom.insertList _radixBloom $ root'':root':roots
   buffer = merkleSpoof root'' parent $ storeHot root'' node'' $ storeHot root' node' _radixBuffer
   nonce = _radixNonce + 2
   state = bool _radixRoot root'' $ isNothing parent

{-# INLINABLE insertRadixTreeBefore #-}

--------------------------------------------------------------------------------

-- TODO (enzo): Documentation.
insertRadixTreeBetween
   :: RadixSearchResult -- ^ Search result.
   -> ByteString -- ^ Value.
   -> RadixTree database -- ^ Radix tree.
   -> RadixTree database
insertRadixTreeBetween (_:|roots, node:|nodes, prefix:|_, prefixOverflow, keyOverflow, cache) value tree@RadixTree {..} =
   seq bloom $ setBloom bloom $ setBuffer buffer $ setCache cache $ setNonce nonce $ setRoot state tree
   where
   prefix' = createPrefix $ drop 1 keyOverflow
   node' = setPrefix prefix' $ Just value `setLeaf` def
   root' = createRootFromNonce _radixNonce
   prefix'' = createPrefix $ drop 1 prefixOverflow
   node'' = setPrefix prefix'' node
   root'' = createRootFromNonce $ _radixNonce + 1
   prefix''' = createPrefix $ drop 1 prefix `bool` prefix $ isNothing parent
   node''' = setPrefix prefix''' $ setChildren children def
   root''' = createRootFromNonce $ _radixNonce + 2
   test = head keyOverflow
   children = bool id swap test (Just root', Just root'')
   parent = listToMaybe $ zip3 roots nodes prefix
   bloom = flip Bloom.insertList _radixBloom $ root''':root'':root':roots
   buffer = merkleSpoof root''' parent $ storeHot root''' node''' $ storeHot root'' node'' $ storeHot root' node' _radixBuffer
   nonce = _radixNonce + 3
   state = bool _radixRoot root''' $ isNothing parent

{-# INLINABLE insertRadixTreeBetween #-}

--------------------------------------------------------------------------------

-- |
-- Delete a value from a radix tree.
deleteRadixTree
   :: RadixDatabase m database
   => ByteString -- ^ Key.
   -> RadixTree database -- ^ Radix tree.
   -> m (RadixTree database)
deleteRadixTree key tree@RadixTree {..} =
   if isEmptyRadixTree tree
   then pure tree
   else searchNonMerkleizedRadixTree key tree >>= \ case
      Left err -> throw err
      Right result@(_, nodes, prefix:|_, [], [], cache) ->
         case nodes of
            -- No children and no parent.
            RadixNode _ Nothing Nothing _:|[] ->
               pure $ deleteRadixTreeNoChildrenNoParent result tree
            -- No children and parent with leaf.
            RadixNode _ Nothing Nothing _:|parent:_ | isJust $ getLeaf parent ->
               pure $ deleteRadixTreeNoChildrenParentWithLeaf result tree
            -- No children and parent without leaf.
            RadixNode _ Nothing Nothing _:|parent:_ -> do
               let test = not $ head prefix
               let root = fromJust $ getChild test parent
               loadHot root _radixBuffer cache _radixDatabase >>= \ case
                  Nothing -> throw $ StateRootDoesNotExist root
                  Just (node, cache') ->
                     pure $ deleteRadixTreeNoChildrenParentWithoutLeaf result node cache' test tree
            -- One left child.
            RadixNode _ child Nothing _:|_ | isJust child -> do
               let test = False
               let root = fromJust child
               loadHot root _radixBuffer cache _radixDatabase >>= \ case
                  Nothing -> throw $ StateRootDoesNotExist root
                  Just (node, cache') ->
                     pure $ deleteRadixTreeOneChild result node cache' test tree
            -- One right child.
            RadixNode _ Nothing child _:|_ | isJust child -> do
               let test = True
               let root = fromJust child
               loadHot root _radixBuffer cache _radixDatabase >>= \ case
                  Nothing -> throw $ StateRootDoesNotExist root
                  Just (node, cache') ->
                     pure $ deleteRadixTreeOneChild result node cache' test tree
            -- Two children.
            _ -> pure $ deleteRadixTreeTwoChildren result tree
      Right _ -> pure tree

{-# SPECIALISE deleteRadixTree
               :: ByteString
               -> RadixTree LevelDB.DB
               -> ResourceT IO (RadixTree LevelDB.DB) #-}

--------------------------------------------------------------------------------

-- TODO (enzo): Documentation.
deleteRadixTreeNoChildrenNoParent
   :: RadixSearchResult -- ^ Search result.
   -> RadixTree database -- ^ Radix tree.
   -> RadixTree database
deleteRadixTreeNoChildrenNoParent (_, _, _, _, _, cache) tree@RadixTree {..} =
   seq bloom $ setBloom bloom $ setBuffer buffer $ setCache cache $ setRoot state tree
   where
   bloom = Bloom.insert defaultRoot _radixBloom
   buffer = storeHot defaultRoot def _radixBuffer
   state = defaultRoot

{-# INLINABLE deleteRadixTreeNoChildrenNoParent #-}

--------------------------------------------------------------------------------

-- TODO (enzo): Documentation.
deleteRadixTreeNoChildrenParentWithLeaf
   :: RadixSearchResult -- ^ Search result.
   -> RadixTree database -- ^ Radix tree.
   -> RadixTree database
deleteRadixTreeNoChildrenParentWithLeaf (_:|_:roots, _:|node:nodes, prefix:|prefixes, _, _, cache) tree@RadixTree {..} =
   seq bloom $ setBloom bloom $ setBuffer buffer $ setCache cache $ setNonce nonce $ setRoot state tree
   where
   node' = setChild test Nothing node
   root' = createRootFromNonce _radixNonce
   test = head prefix
   parent = listToMaybe $ zip3 roots nodes $ map head prefixes
   bloom = flip Bloom.insertList _radixBloom $ root':roots
   buffer = merkleSpoof root' parent $ storeHot root' node' _radixBuffer
   nonce = _radixNonce + 1
   state = bool _radixRoot root' $ isNothing parent
deleteRadixTreeNoChildrenParentWithLeaf _ _ =
   throw $ InvalidArgument "unknown parent"

{-# INLINABLE deleteRadixTreeNoChildrenParentWithLeaf #-}

--------------------------------------------------------------------------------

-- TODO (enzo): Documentation.
deleteRadixTreeNoChildrenParentWithoutLeaf
   :: RadixSearchResult -- ^ Search result.
   -> RadixNode -- ^ Radix node.
   -> RadixCache -- ^ Radix cache.
   -> Bool -- ^ Lineage.
   -> RadixTree database -- ^ Radix tree.
   -> RadixTree database
deleteRadixTreeNoChildrenParentWithoutLeaf (_:|_:roots, _:|_:nodes, _:|prefixes, _, _, _) node@RadixNode {..} cache test tree@RadixTree {..} =
   seq bloom $ setBloom bloom $ setBuffer buffer $ setCache cache $ setNonce nonce $ setRoot state tree
   where
   prefix' = createPrefix $ drop 1 bits `bool` bits $ isNothing parent
   node' = setPrefix prefix' node
   root' = createRootFromNonce _radixNonce
   bits = head prefixes ++ test:maybe [] toBits _radixPrefix
   parent = listToMaybe $ zip3 roots nodes $ map head prefixes
   bloom = flip Bloom.insertList _radixBloom $ root':roots
   buffer = merkleSpoof root' parent $ storeHot root' node' _radixBuffer
   nonce = _radixNonce + 1
   state = bool _radixRoot root' $ isNothing parent
deleteRadixTreeNoChildrenParentWithoutLeaf _ _ _ _ _ =
   throw $ InvalidArgument "unknown parent"

{-# INLINABLE deleteRadixTreeNoChildrenParentWithoutLeaf #-}

--------------------------------------------------------------------------------

-- TODO (enzo): Documentation.
deleteRadixTreeOneChild
   :: RadixSearchResult -- ^ Search result.
   -> RadixNode -- ^ Radix node.
   -> RadixCache -- ^ Radix cache.
   -> Bool -- ^ Lineage.
   -> RadixTree database -- ^ Radix tree.
   -> RadixTree database
deleteRadixTreeOneChild (_:|roots, _:|nodes, prefix:|_, _, _, _) node@RadixNode {..} cache test tree@RadixTree {..} =
   seq bloom $ setBloom bloom $ setBuffer buffer $ setCache cache $ setNonce nonce $ setRoot state tree
   where
   prefix' = createPrefix $ drop 1 bits `bool` bits $ isNothing parent
   node' = setPrefix prefix' node
   root' = createRootFromNonce _radixNonce
   bits = prefix ++ test:maybe [] toBits _radixPrefix
   parent = listToMaybe $ zip3 roots nodes prefix
   bloom = flip Bloom.insertList _radixBloom $ root':roots
   buffer = merkleSpoof root' parent $ storeHot root' node' _radixBuffer
   nonce = _radixNonce + 1
   state = bool _radixRoot root' $ isNothing parent

{-# INLINABLE deleteRadixTreeOneChild #-}

--------------------------------------------------------------------------------

-- TODO (enzo): Documentation.
deleteRadixTreeTwoChildren
   :: RadixSearchResult -- ^ Search result.
   -> RadixTree database -- ^ Radix tree.
   -> RadixTree database
deleteRadixTreeTwoChildren (_:|roots, node:|nodes, prefix:|_, _, _, cache) tree@RadixTree {..} =
   seq bloom $ setBloom bloom $ setBuffer buffer $ setCache cache $ setNonce nonce $ setRoot state tree
   where
   node' = setLeaf Nothing node
   root' = createRootFromNonce _radixNonce
   parent = listToMaybe $ zip3 roots nodes prefix
   bloom = flip Bloom.insertList _radixBloom $ root':roots
   buffer = merkleSpoof root' parent $ storeHot root' node' _radixBuffer
   nonce = _radixNonce + 1
   state = bool _radixRoot root' $ isNothing parent

{-# INLINABLE deleteRadixTreeTwoChildren #-}

--------------------------------------------------------------------------------

-- |
-- Lookup a value in a radix tree.
lookupRadixTree
   :: RadixDatabase m database
   => ByteString -- ^ Key.
   -> RadixTree database -- ^ Radix tree.
   -> m (Maybe (ByteString, RadixTree database))
lookupRadixTree key tree = do
   found <- searchNonMerkleizedRadixTree key tree
   case found of
      Left err -> throw err
      Right (_, RadixNode {..}:|_, _, prefixOverflow, keyOverflow, cache') ->
         if not $ null prefixOverflow && null keyOverflow
         then pure Nothing
         else pure $ do
            value <- _radixLeaf
            let tree' = setCache cache' tree
            pure (value, tree')

{-# SPECIALISE lookupRadixTree
               :: ByteString
               -> RadixTree LevelDB.DB
               -> ResourceT IO (Maybe (ByteString, RadixTree LevelDB.DB)) #-}

--------------------------------------------------------------------------------

-- |
-- Mask a node in a Merkleized radix tree.
merkleSpoof
   :: RadixRoot -- ^ State root.
   -> Maybe (RadixRoot, RadixNode, Bool) -- ^ Parent.
   -> RadixBuffer -- ^ Buffer.
   -> RadixBuffer
merkleSpoof mask = \ case
   Nothing -> id
   Just (root, node, test) ->
      storeHot root $ test `setChild` Just mask $ node

{-# INLINABLE merkleSpoof #-}

--------------------------------------------------------------------------------

-- |
-- Merkleize a radix tree. This will flush the buffer to the database.
merkleizeRadixTree
   :: RadixDatabase m database
   => RadixTree database-- ^ Radix tree.
   -> m (RadixRoot, RadixTree database)
merkleizeRadixTree RadixTree {..} = do
   (root, cache) <- loop _radixRoot _radixCache
   let tree = RadixTree bloom _radixBloomSize Map.empty cache _radixCacheSize root _radixDatabase 0 root
   pure (root, tree)
   where
   bloom = emptyRadixBloom _radixBloomSize
   loop root cache =
      if not $ Bloom.elem root _radixBloom
      then pure (root, cache)
      else do
         -- Load the root node.
         result <- loadHot root _radixBuffer cache _radixDatabase
         case result of
            Nothing -> throw $ StateRootDoesNotExist root
            Just (node@RadixNode {..}, cache') ->
               case (_radixLeft, _radixRight) of
                  -- No children.
                  (Nothing, Nothing) ->
                     storeCold node cache' _radixDatabase
                  -- One left child.
                  (Just child, Nothing) -> do
                     (root', cache'') <- loop child cache'
                     let node' = False `setChild` Just root' $ node
                     storeCold node' cache'' _radixDatabase
                  -- One right child.
                  (Nothing, Just child) -> do
                     (root', cache'') <- loop child cache'
                     let node' = True `setChild` Just root' $ node
                     storeCold node' cache'' _radixDatabase
                  -- Two children.
                  (Just left, Just right) -> do
                     (root', cache'') <- loop left cache'
                     (root'', cache''') <- loop right cache''
                     let node' = setChildren (Just root', Just root'') node
                     storeCold node' cache''' _radixDatabase

{-# SPECIALISE merkleizeRadixTree
               :: RadixTree LevelDB.DB
               -> ResourceT IO (RadixRoot, RadixTree LevelDB.DB) #-}

--------------------------------------------------------------------------------

-- |
-- Get the contents of a radix tree.
contentsRadixTree'
   :: RadixDatabase m database
   => Bool -- ^ Overwrite state root?
   -> (RadixTree database -> m (Maybe (RadixNode, RadixCache))) -- ^ Loading strategy.
   -> RadixTree database -- ^ Radix tree.
   -> m [(ByteString, ByteString)]
contentsRadixTree' flag strategy = \ tree@RadixTree {..} -> do
   let tree' = tree `bool` setRoot _radixCheckpoint tree $ flag
   loop tree' [] [] where
   loop tree@RadixTree {..} prefix accum = do
      result <- strategy tree
      case fst <$> result of
         Nothing -> throw $ StateRootDoesNotExist _radixRoot
         Just RadixNode {..} -> do
            let prefix' = prefix ++ maybe [] toBits _radixPrefix
            let key = fromBits prefix'
            let accum' = maybe accum (\ value -> (key, value):accum) _radixLeaf
            let children = [(,False) <$> _radixLeft, (,True) <$> _radixRight]
            flip foldM accum' `flip` children $ \ accum'' -> \ case
               Nothing -> pure accum''
               Just (root, test) -> do
                  let tree' = setRoot root tree
                  let prefix'' = prefix' ++ [test]
                  loop tree' prefix'' accum''

{-# SPECIALISE contentsRadixTree'
               :: Bool
               -> (RadixTree LevelDB.DB
                   -> ResourceT IO (Maybe (RadixNode, RadixCache)))
               -> RadixTree LevelDB.DB
               -> ResourceT IO [(ByteString, ByteString)] #-}

--------------------------------------------------------------------------------

-- |
-- A convenient alias for `contentsNonMerkleizedRadixTree`.
contentsRadixTree
   :: RadixDatabase m database
   => RadixTree database -- ^ Radix tree.
   -> m [(ByteString, ByteString)]
contentsRadixTree = contentsNonMerkleizedRadixTree

{-# SPECIALISE contentsRadixTree
               :: RadixTree LevelDB.DB
               -> ResourceT IO [(ByteString, ByteString)] #-}

--------------------------------------------------------------------------------

-- |
-- Get the contents of a Merkleized radix tree.
contentsMerkleizedRadixTree
   :: RadixDatabase m database
   => RadixTree database -- ^ Radix tree.
   -> m [(ByteString, ByteString)]
contentsMerkleizedRadixTree =
   contentsRadixTree' True $ \ RadixTree {..} ->
      loadCold _radixRoot _radixCache _radixDatabase

{-# SPECIALISE contentsMerkleizedRadixTree
               :: RadixTree LevelDB.DB
               -> ResourceT IO [(ByteString, ByteString)] #-}

--------------------------------------------------------------------------------

-- |
-- Get the contents of a non-Merkleized radix tree.
contentsNonMerkleizedRadixTree
   :: RadixDatabase m database
   => RadixTree database -- ^ Radix tree.
   -> m [(ByteString, ByteString)]
contentsNonMerkleizedRadixTree =
   contentsRadixTree' False $ \ RadixTree {..} ->
      loadHot _radixRoot _radixBuffer _radixCache _radixDatabase

{-# SPECIALISE contentsNonMerkleizedRadixTree
               :: RadixTree LevelDB.DB
               -> ResourceT IO [(ByteString, ByteString)] #-}

--------------------------------------------------------------------------------

-- |
-- Print a radix tree.
printRadixTree'
   :: MonadIO m
   => RadixDatabase m database
   => Bool -- ^ Overwrite state root?
   -> (RadixTree database -> m (Maybe (RadixNode, RadixCache))) -- ^ Loading strategy.
   -> RadixTree database -- ^ Radix tree.
   -> m ()
printRadixTree' flag strategy = \ tree@RadixTree {..} -> do
   let tree' = tree `bool` setRoot _radixCheckpoint tree $ flag
   loop tree' 0 where
   loop tree@RadixTree {..} i = do
      result <- strategy tree
      case fst <$> result of
         Nothing -> throw $ StateRootDoesNotExist _radixRoot
         Just node@RadixNode {..} -> do
            let indent = (++) $ concat $ replicate i "｜"
            liftIO $ putStrLn $ indent $ show node
            let j = i + 1
            forM_ [_radixLeft, _radixRight] $ \ case
               Nothing -> pure ()
               Just root -> setRoot root tree `loop` j

{-# SPECIALISE printRadixTree'
               :: Bool
               -> (RadixTree LevelDB.DB
                   -> ResourceT IO (Maybe (RadixNode, RadixCache)))
               -> RadixTree LevelDB.DB
               -> ResourceT IO () #-}

--------------------------------------------------------------------------------

-- |
-- A convenient alias for `printNonMerkleizedRadixTree`.
printRadixTree
   :: MonadIO m
   => RadixDatabase m database
   => RadixTree database -- ^ Radix tree.
   -> m ()
printRadixTree = printNonMerkleizedRadixTree

{-# SPECIALISE printRadixTree
               :: RadixTree LevelDB.DB
               -> ResourceT IO () #-}

--------------------------------------------------------------------------------

-- |
-- Print a Merkleized radix tree.
printMerkleizedRadixTree
   :: MonadIO m
   => RadixDatabase m database
   => RadixTree database -- ^ Radix tree.
   -> m ()
printMerkleizedRadixTree =
   printRadixTree' True $ \ RadixTree {..} ->
      loadCold _radixRoot _radixCache _radixDatabase

{-# SPECIALISE printMerkleizedRadixTree
               :: RadixTree LevelDB.DB
               -> ResourceT IO () #-}

--------------------------------------------------------------------------------

-- |
-- Print a non-Merkleized radix tree.
printNonMerkleizedRadixTree
   :: MonadIO m
   => RadixDatabase m database
   => RadixTree database -- ^ Radix tree.
   -> m ()
printNonMerkleizedRadixTree =
   printRadixTree' False $ \ RadixTree {..} ->
      loadHot _radixRoot _radixBuffer _radixCache _radixDatabase

{-# SPECIALISE printNonMerkleizedRadixTree
               :: RadixTree LevelDB.DB
               -> ResourceT IO () #-}

--------------------------------------------------------------------------------
