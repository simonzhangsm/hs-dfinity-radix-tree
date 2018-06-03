{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS -Wall #-}
{-# OPTIONS -fno-warn-missing-signatures #-}

module Network.DFINITY.RadixTree.Lenses
   ( radixBitLen
   , radixName
   , radixPrefix
   , radixLeft
   , radixRight
   , radixChild
   , radixLeaf
   , radixBloom
   , radixBloomBits
   , radixBuffer
   , radixCache
   , radixCheckpoint
   , radixDatabase
   , radixRoot
   ) where

import Data.Bool (bool)
import Lens.Simple (Lens', makeLenses)

import Network.DFINITY.RadixTree.Types

makeLenses ''RadixPrefix
makeLenses ''RadixBranch
makeLenses ''RadixTree

radixChild :: Bool -> Lens' RadixBranch (Maybe RadixRoot)
radixChild = bool radixLeft radixRight
