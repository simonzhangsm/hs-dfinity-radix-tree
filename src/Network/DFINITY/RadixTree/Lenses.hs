{-# LANGUAGE TemplateHaskell #-}

module Network.DFINITY.RadixTree.Lenses
   ( radixBitLen
   , radixName
   , radixPrefix
   , radixLeft
   , radixRight
   , radixLeaf
   , radixCache
   , radixCheckpoint
   , radixDatabase
   , radixRoot
   ) where

import Lens.Simple (makeLenses)

import Network.DFINITY.RadixTree.Types

makeLenses ''RadixPrefix
makeLenses ''RadixBranch
makeLenses ''RadixTree
