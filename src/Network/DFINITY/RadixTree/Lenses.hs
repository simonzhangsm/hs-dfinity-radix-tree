{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS -Wall #-}
{-# OPTIONS -fno-warn-missing-signatures #-}

module Network.DFINITY.RadixTree.Lenses
   ( radixBitLen
   , radixName
   , radixPrefix
   , radixLeft
   , radixRight
   , radixLeaf
   , radixBloom
   , radixBloomBits
   , radixBuffer
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
