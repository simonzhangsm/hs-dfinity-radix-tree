module Network.DFINITY.RadixTree.Bits
   ( Bitable(..)
   , matchBits
   ) where

import Data.Bits ((.|.), setBit, testBit)
import Data.Bool (bool)
import Data.ByteString (ByteString, pack, unpack)
import Data.Word (Word8)

class Bitable a where
   fromBits :: [Bool] -> a
   toBits :: a -> [Bool]

instance Bitable ByteString where
   fromBits = pack . go where
      go [] = []
      go xs = let (a, b) = splitAt 8 xs in toByte a : go b
   toBits = concatMap fromByte . unpack

matchBits :: [Bool] -> [Bool] -> [Bool]
matchBits x = map fst . zipWhile (==) x

zipWhile :: (a -> b -> Bool) -> [a] -> [b] -> [(a,b)]
zipWhile f x y = takeWhile (uncurry f) $ zip x y

fromByte :: Word8 -> [Bool]
fromByte = flip map order . testBit

toByte :: [Bool] -> Word8
toByte = foldl (.|.) 0 . zipWith (bool 0 . setBit 0) order

order :: Enum a => Num a => [a]
order = [7, 6.. 0]
