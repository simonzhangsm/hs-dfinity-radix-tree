{-# OPTIONS -Wall #-}

module Network.DFINITY.RadixTree.Utilities
   ( createPrefix
   , createRoot
   , defaultRoot
   ) where

import Codec.Serialise (serialise)
import Crypto.Hash.SHA256 (hashlazy)
import Data.ByteString.Char8 as Byte (take)
import Data.ByteString.Lazy.Char8 (toStrict)
import Data.ByteString.Short (toShort)
import Data.Default.Class (def)

import Network.DFINITY.RadixTree.Bits
import Network.DFINITY.RadixTree.Types

createPrefix :: [Bool] -> Maybe RadixPrefix
createPrefix bits =
   if null bits
   then Nothing
   else Just $ fromBits bits

createRoot :: RadixBranch -> RadixRoot
createRoot = toShort . Byte.take 20 . hashlazy . serialise

defaultRoot :: RadixRoot
defaultRoot = createRoot def
