{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Network.DFINITY.RadixTree
   ( Network.DFINITY.RadixTree.lookup
   ) where

import Codec.Serialise (deserialise)
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Char8 (fromStrict)

import Network.DFINITY.RadixTree.Bits
import Network.DFINITY.RadixTree.Memory
import Network.DFINITY.RadixTree.Types

lookup :: RadixTree -> ByteString -> IO (Maybe ByteString)
lookup tree = go tree . toBits
   where
   go RadixTree {..} = \ case
      [] -> pure _radixLeaf
      test:residue ->
         case bool _radixLeft _radixRight test of
            Nothing -> pure Nothing
            Just key -> do
               result <- load _radixCache _radixDatabase key
               case result of
                  Nothing -> pure Nothing
                  Just value -> do
                     let root = deserialise $ fromStrict value
                     flip go residue $ RadixTree _radixCache _radixDatabase root
      where
      RadixBranch {..} = _radixRoot
