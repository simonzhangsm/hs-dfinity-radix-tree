{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE  RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Network.DFINITY.RadixTree
   {- ( createRadixTree
   , lookupRadixTree
   , insertRadixTree
   , deleteRadixTree
   , printRadixTree
   ) -} where

import Codec.Serialise (serialise)
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Resource (MonadResource)
import Crypto.Hash.SHA256 (hash)
import Data.Bool (bool)
import Data.ByteString.Base16 (encode)
import Data.ByteString.Char8 as Byte (ByteString, take, unpack)
import Data.ByteString.Lazy.Char8 (toStrict)
import Data.ByteString.Short (toShort)
import Data.Default.Class (def)
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty(..), fromList)
import Data.LruCache as LRU (empty)
import Data.Map.Strict as Map (delete, empty, insert)
import Data.Maybe (fromJust, listToMaybe, isNothing)
import Database.LevelDB (Options(..), open)
import Lens.Simple (set)
import Text.Printf (printf)

import Network.DFINITY.RadixTree.Bits
import Network.DFINITY.RadixTree.Lenses
import Network.DFINITY.RadixTree.Memory
import Network.DFINITY.RadixTree.Types





















--------------------------------------------------------------------------------

-- |
-- Create a radix tree.
createRadixTree
   :: MonadResource m
   => Int -- ^ Cache size.
   -> FilePath -- ^ Database.
   -> Maybe ByteString -- ^ State root.
   -> m RadixTree
createRadixTree size path checkpoint
   | size <= 0 = fail "createRadixTree: invalid cache size"
   | otherwise = do
      database <- open path $ def {createIfMissing = True}
      (root, cache') <-
         case checkpoint of
            Nothing -> storeCold cache database def
            Just root -> do
               result <- loadCold cache database root
               case snd <$> result of
                  Nothing -> fail "createRadixTree: state root does not exist"
                  Just cache' -> pure (root, cache')
      pure $ RadixTree Map.empty cache' root database root
      where cache = LRU.empty size

-- |
-- Search for a value in the radix tree.
searchRadixTree
   :: MonadIO m
   => (RadixTree -> m (Maybe (RadixBranch, RadixCache))) -- ^ Load strategy.
   -> ByteString -- ^ Key.
   -> RadixTree -- ^ Radix tree.
   -> m (Maybe (NonEmpty ByteString, NonEmpty RadixBranch, NonEmpty [Bool], [Bool], [Bool], RadixCache))
searchRadixTree load = flip $ \ tree ->
   go tree Nothing [] [] [] . toBits where
   go tree@RadixTree {..} implicit branches roots prefixes key = do
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
               let tree' = set radixRoot root' $ set radixCache cache' tree
               let implicit' = Just bit
               go tree' implicit' branches' roots' prefixes' key'

--------------------------------------------------------------------------------

searchRadixTreeHot
   :: MonadIO m
   => ByteString -- ^ Key.
   -> RadixTree -- ^ Radix tree.
   -> m (Maybe (NonEmpty ByteString, NonEmpty RadixBranch, NonEmpty [Bool], [Bool], [Bool], RadixCache))
searchRadixTreeHot =
   searchRadixTree $ \ RadixTree {..} ->
      case loadHot _radixBuffer _radixRoot of
         Nothing -> loadCold _radixCache _radixDatabase _radixRoot
         Just branch -> pure $ Just (branch, _radixCache)

searchRadixTreeCold
   :: MonadIO m
   => ByteString -- ^ Key.
   -> RadixTree -- ^ Radix tree.
   -> m (Maybe (NonEmpty ByteString, NonEmpty RadixBranch, NonEmpty [Bool], [Bool], [Bool], RadixCache))
searchRadixTreeCold =
   searchRadixTree $ \ RadixTree {..} ->
      loadCold _radixCache _radixDatabase _radixRoot

merkleSpoof
   :: Maybe (ByteString, RadixBranch, Bool) -- ^ Parent.
   -> ByteString -- ^ Child.
   -> RadixBuffer -- ^ Buffer.
   -> RadixBuffer
merkleSpoof parent child buffer =
   case parent of
      Nothing -> buffer
      Just (root, branch, test) ->
         let lens = set $ bool radixLeft radixRight test
         in Map.insert (toShort root) (toStrict $ serialise $ Just child `lens` branch) buffer

-- |
-- Lookup a value in the radix tree.
lookupRadixTree
   :: MonadIO m
   => ByteString -- ^ Key.
   -> RadixTree -- ^ Radix tree.
   -> m (Maybe (ByteString, RadixTree))
lookupRadixTree key tree = do
   result <- searchRadixTreeHot key tree
   case result of
      Nothing -> fail "lookupRadixTree: state root does not exist"
      Just (_, RadixBranch {..} :| _, _, prefixOverflow, keyOverflow, cache') -> do
         let tree' = set radixCache cache' tree
         pure $ (,tree') <$> _radixLeaf

-- |
-- Insert a key and value into a radix tree.
insertRadixTree
   :: MonadIO m
   => ByteString -- ^ Key.
   -> ByteString -- ^ Value.
   -> RadixTree -- ^ Radix tree.
   -> m RadixTree
insertRadixTree key value tree@RadixTree {..} =
   if _radixRoot == defaultRoot
   then do
      -- Base case.
      let prefix' = toPrefix $ toBits key
      let branch' = RadixBranch prefix' Nothing Nothing (Just value)
      let (root', buffer') = storeHot _radixBuffer branch'
      pure $ RadixTree buffer' _radixCache _radixCheckpoint _radixDatabase root'
   else do
      result <- searchRadixTreeHot key tree
      case result of
         Nothing -> fail "insertRadixTree: state root does not exist"
         Just (root :| roots, RadixBranch {..} :| branches, prefix :| prefixes, prefixOverflow, keyOverflow, cache) -> do
            let parent = listToMaybe $ zip3 roots branches [head prefix]
            let buffer = flip Map.delete _radixBuffer $ toShort root
            if null prefixOverflow
            then if null keyOverflow
               then do
                  -- Update case.
                  let branch' = RadixBranch _radixPrefix _radixLeft _radixRight (Just value)
                  let (root', buffer') = storeHot buffer branch'
                  let buffer'' = merkleSpoof parent root' buffer'
                  pure $ RadixTree buffer'' cache _radixCheckpoint _radixDatabase $ _radixRoot `bool` root' $ root == _radixRoot
             else do
                  -- Key overflow case.
                  let prefix' = toPrefix $ drop 1 keyOverflow
                  let branch' = RadixBranch prefix' Nothing Nothing (Just value)
                  let (root', buffer') = storeHot buffer branch'
                  let branch'' = if head keyOverflow
                        then RadixBranch _radixPrefix _radixLeft (Just root') _radixLeaf
                        else RadixBranch _radixPrefix (Just root') _radixRight _radixLeaf
                  let (root'', buffer'') = storeHot buffer' branch''
                  let buffer''' = merkleSpoof parent root'' buffer''
                  pure $ RadixTree buffer''' cache _radixCheckpoint _radixDatabase $_radixRoot `bool` root'' $ root == _radixRoot
            else if null keyOverflow
               then do
                  -- Prefix overflow case.
                  let prefix' = toPrefix $ drop 1 prefixOverflow
                  let branch' = RadixBranch prefix' _radixLeft _radixRight _radixLeaf
                  let (root', buffer') = storeHot buffer branch'
                  let prefix'' = toPrefix $ drop 1 prefix `bool` prefix $ root == _radixRoot
                  let branch'' = if head prefixOverflow
                        then RadixBranch prefix'' Nothing (Just root') (Just value)
                        else RadixBranch prefix'' (Just root') Nothing (Just value)
                  let (root'', buffer'') = storeHot buffer' branch''
                  let buffer''' = merkleSpoof parent root'' buffer''
                  pure $ RadixTree buffer''' cache _radixCheckpoint _radixDatabase $ _radixRoot `bool` root'' $ root == _radixRoot
               else do
                  -- General case.
                  let prefix' = toPrefix $ drop 1 keyOverflow
                  let branch' = RadixBranch prefix' Nothing Nothing $ Just value
                  let (root', buffer') = storeHot buffer branch'
                  let prefix'' = toPrefix $ drop 1 prefixOverflow
                  let branch'' = RadixBranch prefix'' _radixLeft _radixRight _radixLeaf
                  let (root'', buffer'') = storeHot buffer' branch''
                  let prefix''' = toPrefix $ drop 1 prefix `bool` prefix $ root == _radixRoot
                  let branch''' = if head keyOverflow
                        then RadixBranch prefix''' (Just root'') (Just root') Nothing
                        else RadixBranch prefix''' (Just root') (Just root'') Nothing
                  let (root''', buffer''') = storeHot buffer'' branch'''
                  let buffer'''' = merkleSpoof parent root''' buffer'''
                  pure $ RadixTree buffer'''' cache _radixCheckpoint _radixDatabase $ _radixRoot `bool` root''' $ root == _radixRoot

merkleizeRadixTree
   :: RadixTree -- ^ Radix tree.
   -> m RadixTree
merkleizeRadixTree RadixTree {..} = undefined

{-
-- |
-- Merkleize a path in a radix tree.
merkleize
   :: MonadIO m
   => [(ByteString, RadixBranch, Bool)] -- ^ Path.
   -> RadixTree -- ^ Radix tree.
   -> m RadixTree
merkleize = flip $ \ tree@RadixTree {..} -> \ case
   [] -> pure tree
   (root, branch, test):path' -> do
      let lens = set $ bool radixLeft radixRight test
      let branch' = Just _radixRoot `lens` branch
      (cache', root') <- store _radixCache _radixDatabase branch'
      let deletes' = root:_radixDeletes
      let inserts' = root':_radixInserts
      let tree' = RadixTree cache' _radixDatabase deletes' inserts' root'
      merkleize path' tree'
-}

-- |
--  Print a radix tree.
printRadixTree
   :: MonadIO m
   => RadixTree -- ^ Radix tree.
   -> m ()
printRadixTree = \ tree@RadixTree {..} -> do
   go 0 tree where
   go i tree@RadixTree {..} = do
      result <-
         case loadHot _radixBuffer _radixRoot of
            Nothing -> loadCold _radixCache _radixDatabase _radixRoot
            Just branch -> pure $ Just (branch, _radixCache)
      case fst <$> result of
         Nothing -> fail "printRadixTree: state root does not exist"
         Just branch@RadixBranch {..} -> do
            let tab = (++) $ concat $ replicate i "｜"
            liftIO $ putStrLn $ tab $ show branch
            let j = i + 1
            forM_ [_radixLeft, _radixRight] $ \ case
               Nothing -> pure ()
               Just root -> go j $ set radixRoot root tree



-- |
-- The default state root.
defaultRoot :: ByteString
defaultRoot = Byte.take 20 $ hash $ toStrict $ serialise branch
   where
   branch = def :: RadixBranch

toPrefix :: [Bool] -> Maybe RadixPrefix
toPrefix bits =
   if null bits
   then Nothing
   else Just $ fromBits bits
