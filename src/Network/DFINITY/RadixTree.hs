{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Network.DFINITY.RadixTree
   ( createRadixTree
   , lookupRadixTree
   , insertRadixTree
   , deleteRadixTree
   , printRadixTree
   ) where

import Codec.Serialise (serialise)
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Resource (MonadResource)
import Crypto.Hash.SHA256 (hash)
import Data.Bool (bool)
import Data.ByteString.Char8 as Byte (ByteString, take)
import Data.ByteString.Lazy.Char8 (toStrict)
import Data.Default.Class (def)
import Data.LruCache (empty)
import Database.LevelDB as Level (DB, Options(..), open)
import Lens.Simple (set)

import Network.DFINITY.RadixTree.Bits
import Network.DFINITY.RadixTree.Lenses
import Network.DFINITY.RadixTree.Memory
import Network.DFINITY.RadixTree.Types

-- |
-- Create a radix tree.
createRadixTree
   :: MonadResource m
   => Int -- ^ Cache size.
   -> FilePath -- ^ Database.
   -> Maybe ByteString -- ^ State root.
   -> m RadixTree
createRadixTree size path state =
   if size < 1
   then fail "createRadixTree: invalid cache size"
   else do
      let cache = empty size
      database <- Level.open path $ def {createIfMissing = True}
      case state of
         Nothing -> do
            (cache', root) <- store cache database def
            pure $ RadixTree cache' database root
         Just root -> do
            result <- load cache database root
            case result of
               Nothing -> fail "createRadixTree: state root does not exist"
               _ -> pure $ RadixTree cache database root

-- |
-- Search for a value in the radix tree.
searchRadixTree
   :: MonadIO m
   => ByteString -- ^ Key.
   -> RadixTree -- ^ Radix tree.
   -> m (RadixBranch, [ByteString], [[Bool]], [Bool], [Bool])
searchRadixTree = flip $ \ tree ->
   go tree Nothing [] [] . toBits
   where
   go RadixTree {..} implicit roots prefixes key = do
      -- Load the state root.
      result <- load _radixCache _radixDatabase _radixRoot
      case result of
         Nothing -> fail "searchRadixTree: state root does not exist"
         Just branch@RadixBranch {..} -> do
            -- Calculate the prefix and overflow.
            let bits = maybe id (:) implicit $ maybe [] toBits _radixPrefix
            let prefix = matchBits bits key
            let overflow = length prefix `drop` bits
            -- Update the accumulators.
            let roots' = _radixRoot:roots
            let prefixes' = prefix:prefixes
            let key' = length prefix `drop` key
            -- Check the termination criteria.
            if not (null overflow) || null key'
            then pure (branch, roots', prefixes', overflow, key')
            else case bool _radixLeft _radixRight $ head key' of
               Nothing -> pure (branch, roots', prefixes', overflow, key')
               Just root -> do
                  -- Recurse.
                  let tree' = RadixTree _radixCache _radixDatabase root
                  let implicit' = Just $ head key'
                  go tree' implicit' roots' prefixes' key'

-- |
-- Lookup a value in the radix tree.
lookupRadixTree
   :: MonadIO m
   => ByteString -- ^ Key.
   -> RadixTree -- ^ Radix tree.
   -> m (Maybe ByteString)
lookupRadixTree key tree = do
   (RadixBranch {..}, _, _, prefixOverflow, keyOverflow) <- searchRadixTree key tree
   pure $ bool Nothing _radixLeaf $ null prefixOverflow && null keyOverflow

-- |
-- The default state root.
defaultRoot :: ByteString
defaultRoot = Byte.take 20 $ hash $ toStrict $ serialise branch
   where
   branch = def :: RadixBranch

-- |
-- Merkleize a branch in the radix tree.
merkleize
   :: MonadIO m
   => RadixCache -- ^ Cache.
   -> DB -- ^ Database.
   -> ByteString -- ^ Branch.
   -> [(ByteString, Bool)] -- ^ Path.
   -> m RadixTree
merkleize cache database root path = do
   (cache', root') <- go cache root path
   pure $ RadixTree cache' database root'
   where
   go cache' root' path' =
      if null path'
      then pure (cache', root')
      else do
         let (parent, test):path'' = path'
         result <- load cache' database parent
         case result of
            Nothing -> fail "merkleize: state root does not exist"
            Just branch -> do
               let lens = set $ bool radixLeft radixRight test
               let branch'' = Just root' `lens` branch
               (cache'', root'') <- store cache' database branch''
               go cache'' root'' path''

-- |
-- Insert a key and value into the radix tree.
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
      free _radixDatabase _radixRoot
      let bits = toBits key
      let prefix = if null bits then Nothing else Just $ fromBits bits
      let branch = RadixBranch prefix Nothing Nothing $ Just value
      (cache, root) <- store _radixCache _radixDatabase branch
      pure $ RadixTree cache _radixDatabase root
   else do
      (RadixBranch {..}, roots, prefixes, prefixOverflow, keyOverflow) <- searchRadixTree key tree
      let path = tail roots `zip` map head prefixes
      if null prefixOverflow
      then if null keyOverflow
         then do
            -- Update case.
            free _radixDatabase $ head roots
            let branch = RadixBranch _radixPrefix _radixLeft _radixRight $ Just value
            (cache, root) <- store _radixCache _radixDatabase branch
            merkleize cache _radixDatabase root path
         else do
            -- Key overflow case.
            let prefix = toPrefix $ drop 1 keyOverflow
            let branch = RadixBranch prefix Nothing Nothing $ Just value
            (cache, root) <- Just <$$> store _radixCache _radixDatabase branch
            free _radixDatabase $ head roots
            let branch' = if head keyOverflow
                  then RadixBranch _radixPrefix _radixLeft root _radixLeaf
                  else RadixBranch _radixPrefix root _radixRight _radixLeaf
            (cache', root') <- store cache _radixDatabase branch'
            merkleize cache' _radixDatabase root' path
      else if null keyOverflow
         then do
            -- Prefix overflow case.
            free _radixDatabase $ head roots
            let prefix = toPrefix $ drop 1 prefixOverflow
            let branch = RadixBranch prefix _radixLeft _radixRight _radixLeaf
            (cache, root) <- Just <$$> store _radixCache _radixDatabase branch
            let prefix' = toPrefix $ if head roots == _radixRoot
                  then head prefixes
                  else drop 1 $ head prefixes
            let branch' = if head prefixOverflow
                  then RadixBranch prefix' Nothing root $ Just value
                  else RadixBranch prefix' root Nothing $ Just value
            (cache', root') <- store cache _radixDatabase branch'
            merkleize cache' _radixDatabase root' path
         else do
            -- General case.
            let prefix = toPrefix $ drop 1 keyOverflow
            let branch = RadixBranch prefix Nothing Nothing $ Just value
            (cache, root) <- Just <$$> store _radixCache _radixDatabase branch
            let prefix' = toPrefix $ drop 1 prefixOverflow
            let branch' = RadixBranch prefix' _radixLeft _radixRight _radixLeaf
            (cache', root') <- Just <$$> store cache _radixDatabase branch'
            let prefix'' = toPrefix $ if head roots == _radixRoot
                  then head prefixes
                  else drop 1 $ head prefixes
            let branch'' = if head keyOverflow
                  then RadixBranch prefix'' root' root Nothing
                  else RadixBranch prefix'' root root' Nothing
            (cache'', root'') <- store cache' _radixDatabase branch''
            merkleize cache'' _radixDatabase root'' path
   where
   toPrefix :: [Bool] -> Maybe RadixPrefix
   toPrefix bits =
      if null bits
      then Nothing
      else Just $ fromBits bits

-- |
-- Delete a value from the radix tree.
deleteRadixTree
   :: MonadIO m
   => ByteString -- ^ Key.
   -> RadixTree -- ^ Radix tree.
   -> m RadixTree
deleteRadixTree =
   fail "deleteRadixTree: not implemented"

-- |
--  Print a radix tree.
printRadixTree
   :: MonadIO m
   => RadixTree -- ^ Radix tree.
   -> m ()
printRadixTree =
   go 0 where
   go i RadixTree {..} = do
      result <- load _radixCache _radixDatabase _radixRoot
      case result of
         Nothing -> fail "printRadixTree: state root does not exist"
         Just branch@RadixBranch {..} -> do
            liftIO $ putStrLn $ indent $ show branch
            let j = i + 1
            forM_ [_radixLeft, _radixRight] $ \ case
               Nothing -> pure ()
               Just child -> go j $ RadixTree _radixCache _radixDatabase child
      where
      indent = (++) $ concat $ replicate i "｜"

-- |
-- Promote a function to a nested functor.
(<$$>) :: (Functor f, Functor g) => (a -> b) -> g (f a) -> g (f b)
(<$$>) f = fmap $ fmap f
