{-# OPTIONS -Wall #-}

module Network.DFINITY.RadixTree.Utilities
   ( createPrefix
   , createRoot
   , createRootFromNonce
   , defaultRoot
   ) where

import Codec.Serialise (serialise)
import Crypto.Hash.SHA256 (hashlazy)
import Data.ByteString.Builder (toLazyByteString, wordDec)
import Data.ByteString.Char8 as Byte (take)
import Data.ByteString.Short (toShort)
import Data.Default.Class (def)

import Network.DFINITY.RadixTree.Bits
import Network.DFINITY.RadixTree.Types

createPrefix :: [Bool] -> Maybe RadixPrefix
createPrefix bits =
   if null bits
   then Nothing
   else Just $ fromBits bits

createRoot :: RadixNode -> RadixRoot
createRoot = toShort . Byte.take 20 . hashlazy . serialise

createRootFromNonce :: Word -> RadixRoot
createRootFromNonce = toShort . Byte.take 20 . hashlazy . toLazyByteString . wordDec

defaultRoot :: RadixRoot
defaultRoot = createRoot def
