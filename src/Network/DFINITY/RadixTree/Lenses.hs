{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS -Wall #-}
{-# OPTIONS -fno-warn-missing-signatures #-}
{-# OPTIONS -fno-warn-unused-top-binds #-}

module Network.DFINITY.RadixTree.Lenses
   ( getPrefix
   , getLeft
   , getRight
   , getChild
   , getLeaf
   , getBloom
   , getBuffer
   , getCache
   , getCheckpoint
   , getRoot
   , setPrefix
   , setLeft
   , setRight
   , setChild
   , setChildren
   , setLeaf
   , setBloom
   , setBuffer
   , setCache
   , setCheckpoint
   , setRoot
   ) where

import Data.Bool (bool)
import Data.ByteString (ByteString)
import Lens.Simple (makeLenses, set, view)

import Network.DFINITY.RadixTree.Types

makeLenses ''RadixBranch
makeLenses ''RadixTree

getPrefix :: RadixBranch -> Maybe RadixPrefix
getPrefix = view radixPrefix

getLeft :: RadixBranch -> Maybe RadixRoot
getLeft = view radixLeft

getRight :: RadixBranch -> Maybe RadixRoot
getRight = view radixRight

getChild :: Bool -> RadixBranch -> Maybe RadixRoot
getChild = bool getLeft getRight

getLeaf :: RadixBranch -> Maybe ByteString
getLeaf = view radixLeaf

getBloom :: RadixTree -> RadixBloom
getBloom = view radixBloom

getBuffer :: RadixTree -> RadixBuffer
getBuffer = view radixBuffer

getCache :: RadixTree -> RadixCache
getCache = view radixCache

getCheckpoint :: RadixTree -> RadixRoot
getCheckpoint = view radixCheckpoint

getRoot :: RadixTree -> RadixRoot
getRoot = view radixRoot

setPrefix :: Maybe RadixPrefix -> RadixBranch -> RadixBranch
setPrefix = set radixPrefix

setLeft :: Maybe RadixRoot -> RadixBranch -> RadixBranch
setLeft = set radixLeft

setRight :: Maybe RadixRoot -> RadixBranch -> RadixBranch
setRight = set radixRight

setChild :: Bool -> Maybe RadixRoot -> RadixBranch -> RadixBranch
setChild = bool setLeft setRight

setChildren :: (Maybe RadixRoot, Maybe RadixRoot) -> RadixBranch -> RadixBranch
setChildren (left, right) = setLeft left . setRight right

setLeaf :: Maybe ByteString -> RadixBranch -> RadixBranch
setLeaf = set radixLeaf

setBloom :: RadixBloom -> RadixTree -> RadixTree
setBloom = set radixBloom

setBuffer :: RadixBuffer -> RadixTree -> RadixTree
setBuffer = set radixBuffer

setCache :: RadixCache -> RadixTree -> RadixTree
setCache = set radixCache

setCheckpoint :: RadixRoot -> RadixTree -> RadixTree
setCheckpoint = set radixCheckpoint

setRoot :: RadixRoot -> RadixTree -> RadixTree
setRoot = set radixRoot
