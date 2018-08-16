{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}

{-# OPTIONS -Wall #-}

module Network.DFINITY.RadixTree.Bloom
   ( emptyRadixBloom
   ) where

import Data.BloomFilter (empty)
import Data.ByteString.Short.Internal (ShortByteString(..))
import Data.Word (Word32)
import GHC.Prim (ByteArray#, Int#, (-#), indexWord32Array#, quotInt#, sizeofByteArray#)
import GHC.Word (Word32(..))

import Network.DFINITY.RadixTree.Types

emptyRadixBloom :: Int -> RadixBloom
emptyRadixBloom = empty hash

hash :: RadixRoot -> [Word32]
hash = \ case
   SBS bytes# ->
      let n# = sizeofByteArray# bytes#
          m# = quotInt# n# 4#
      in  hash# bytes# m#

hash# :: ByteArray# -> Int# -> [Word32]
hash# bytes# = \ case
   0# -> []
   i# ->
      let j# = i# -# 1#
          w# = indexWord32Array# bytes# j#
      in  W32# w# : hash# bytes# j#
