{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS -Wall #-}
{-# OPTIONS -fno-warn-missing-signatures #-}
{-# OPTIONS -fno-warn-unused-top-binds #-}

module Network.DFINITY.RadixTree.Lenses
   ( getPrefix
   , getLeft
   , getRight
   , getChild
   , getChildren
   , getLeaf
   , getBloom
   , getBuffer
   , getCache
   , getCheckpoint
   , getNonce
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
   , setNonce
   , setRoot
   ) where

import Data.Bool (bool)
import Data.ByteString (ByteString)
import Lens.Simple (makeLenses, set, view)

import Network.DFINITY.RadixTree.Types

makeLenses ''RadixNode
makeLenses ''RadixTree

getPrefix :: RadixNode -> Maybe RadixPrefix
getPrefix = view radixPrefix

getLeft :: RadixNode -> Maybe RadixRoot
getLeft = view radixLeft

getRight :: RadixNode -> Maybe RadixRoot
getRight = view radixRight

getChild :: Bool -> RadixNode -> Maybe RadixRoot
getChild = bool getLeft getRight

getChildren :: RadixNode -> (Maybe RadixRoot, Maybe RadixRoot)
getChildren node = (getLeft node, getRight node)

getLeaf :: RadixNode -> Maybe ByteString
getLeaf = view radixLeaf

getBloom :: RadixTree database -> RadixBloom
getBloom = view radixBloom

getBuffer :: RadixTree database -> RadixBuffer
getBuffer = view radixBuffer

getCache :: RadixTree database -> RadixCache
getCache = view radixCache

getCheckpoint :: RadixTree database -> RadixRoot
getCheckpoint = view radixCheckpoint

getNonce :: RadixTree database -> Word
getNonce = view radixNonce

getRoot :: RadixTree database -> RadixRoot
getRoot = view radixRoot

setPrefix :: Maybe RadixPrefix -> RadixNode -> RadixNode
setPrefix = set radixPrefix

setLeft :: Maybe RadixRoot -> RadixNode -> RadixNode
setLeft = set radixLeft

setRight :: Maybe RadixRoot -> RadixNode -> RadixNode
setRight = set radixRight

setChild :: Bool -> Maybe RadixRoot -> RadixNode -> RadixNode
setChild = bool setLeft setRight

setChildren :: (Maybe RadixRoot, Maybe RadixRoot) -> RadixNode -> RadixNode
setChildren (left, right) = setLeft left . setRight right

setLeaf :: Maybe ByteString -> RadixNode -> RadixNode
setLeaf = set radixLeaf

setBloom :: RadixBloom -> RadixTree database -> RadixTree database
setBloom = set radixBloom

setBuffer :: RadixBuffer -> RadixTree database -> RadixTree database
setBuffer = set radixBuffer

setCache :: RadixCache -> RadixTree database -> RadixTree database
setCache = set radixCache

setCheckpoint :: RadixRoot -> RadixTree database -> RadixTree database
setCheckpoint = set radixCheckpoint

setNonce :: Word -> RadixTree database -> RadixTree database
setNonce = set radixNonce

setRoot :: RadixRoot -> RadixTree database -> RadixTree database
setRoot = set radixRoot
