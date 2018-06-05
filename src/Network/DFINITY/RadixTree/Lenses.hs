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
   , radixLeaf
   , radixBloom
   , radixBloomBits
   , radixBuffer
   , radixCache
   , radixCheckpoint
   , radixDatabase
   , radixRoot
   , setPrefix
   , setLeft
   , setRight
   , setChild
   , setLeaf
   ) where

import Data.Bool (bool)
import Data.ByteString (ByteString)
import Lens.Simple (makeLenses, set)

import Network.DFINITY.RadixTree.Types

makeLenses ''RadixPrefix
makeLenses ''RadixBranch
makeLenses ''RadixTree

setPrefix :: Maybe RadixPrefix -> RadixBranch -> RadixBranch
setPrefix = set radixPrefix

setLeft :: Maybe RadixRoot -> RadixBranch -> RadixBranch
setLeft = set radixLeft

setRight :: Maybe RadixRoot -> RadixBranch -> RadixBranch
setRight = set radixRight

setChild :: Bool -> Maybe RadixRoot -> RadixBranch -> RadixBranch
setChild =  bool setLeft setRight

setLeaf :: Maybe ByteString -> RadixBranch -> RadixBranch
setLeaf = set radixLeaf
