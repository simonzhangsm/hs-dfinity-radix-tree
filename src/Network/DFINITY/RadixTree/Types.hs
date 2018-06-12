{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS -Wall #-}

module Network.DFINITY.RadixTree.Types
   ( RadixBloom
   , RadixBranch(..)
   , RadixBuffer
   , RadixCache
   , RadixDatabase
   , RadixError(..)
   , RadixPrefix(..)
   , RadixRoot
   , RadixSearchResult
   , RadixTree(..)
   ) where

import Codec.Serialise as CBOR (Serialise(..), serialise)
import Codec.Serialise.Decoding (decodeBytes, decodeInt, decodeListLen)
import Codec.Serialise.Encoding (encodeBytes, encodeInt, encodeListLen)
import Crypto.Hash.SHA256 (hash)
import Control.DeepSeq (NFData(..))
import Control.Exception (Exception)
import Control.Monad (void)
import Data.BloomFilter (Bloom)
import Data.Bool (bool)
import Data.ByteString.Base16 as Base16 (encode)
import Data.ByteString.Char8 (ByteString, unpack)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Short (ShortByteString, fromShort, toShort)
import Data.Data (Data)
import Data.Default.Class (Default(..))
import Data.List.NonEmpty (NonEmpty)
import Data.LruCache (LruCache)
import Data.Map.Strict (Map)
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Database.LevelDB (DB)
import Text.Printf (printf)

import Network.DFINITY.RadixTree.Bits
import Network.DFINITY.RadixTree.Serialise

type RadixBloom = Bloom RadixRoot

data RadixBranch
   = RadixBranch
   { _radixPrefix :: Maybe RadixPrefix
   , _radixLeft :: Maybe RadixRoot
   , _radixRight :: Maybe RadixRoot
   , _radixLeaf :: Maybe ByteString
   } deriving (Data, Eq)

instance NFData RadixBranch where
   rnf RadixBranch {..} =
      rnf _radixPrefix `seq`
      rnf _radixLeft `seq`
      rnf _radixRight `seq`
      rnf _radixLeaf `seq`
      ()

instance Default RadixBranch where
   def = RadixBranch Nothing Nothing Nothing Nothing

instance Serialise RadixBranch where
   encode RadixBranch {..} =
      encodeListLen len <>
      encodeMaybe CBOR.encode _radixPrefix <>
      encodeMaybe encodeSide left <>
      encodeMaybe encodeSide right <>
      maybe mempty encodeBytes _radixLeaf
      where
      len = bool 3 4 $ isJust _radixLeaf
      left = fromShort <$> _radixLeft
      right = fromShort <$> _radixRight
   decode = do
      len <- decodeListLen
      prefix <- decodeMaybe decode
      left <- decodeMaybe $ toShort <$> decodeSide
      right <- decodeMaybe $ toShort <$> decodeSide
      leaf <- decodeLeaf len
      pure $ RadixBranch prefix left right leaf

instance Show RadixBranch where
   show branch@RadixBranch {..} =
      case color 7 . unpack <$> _radixLeaf of
         Nothing -> printf "%s@[%s,%s,%s]" root prefix left right
         Just leaf -> printf "%s@[%s,%s,%s,%s]" root prefix left right leaf
      where
      color :: Int -> String -> String
      color = printf "\ESC[9%dm%s\ESC[0m"
      format = take 8 . unpack . Base16.encode
      root = color 4 $ format $ hash $ toStrict $ serialise branch
      prefix = color 7 $ maybe "null" show _radixPrefix
      left = color 4 $ maybe "null" format $ fromShort <$> _radixLeft
      right = color 4 $ maybe "null" format $ fromShort <$> _radixRight

type RadixBuffer = Map RadixRoot RadixBranch

type RadixCache = LruCache RadixRoot RadixBranch

type RadixDatabase = DB

data RadixError
   = StateRootDoesNotExist
     deriving (Data, Eq, Show)

instance Exception RadixError

data RadixPrefix
   = RadixPrefix
   { _radixBitLen :: Int
   , _radixName :: ByteString
   } deriving (Data, Eq)

instance Bitable RadixPrefix where
   toBits RadixPrefix {..} = take _radixBitLen $ toBits _radixName
   fromBits bits = RadixPrefix bitLen name
      where
      bitLen = length bits
      name = fromBits bits

instance NFData RadixPrefix where
   rnf RadixPrefix {..} =
      rnf _radixBitLen `seq`
      rnf _radixName `seq`
      ()

instance Serialise RadixPrefix where
   encode RadixPrefix {..} =
      encodeListLen 2 <>
      encodeInt _radixBitLen <>
      encodeBytes _radixName
   decode = do
      void decodeListLen
      bitLen <- decodeInt
      name <- decodeBytes
      pure $ RadixPrefix bitLen name

instance Show RadixPrefix where
   show = map compress . toBits
      where compress = bool '0' '1'

type RadixRoot = ShortByteString

type RadixSearchResult = (NonEmpty RadixRoot, NonEmpty RadixBranch, NonEmpty [Bool], [Bool], [Bool], RadixCache)

data RadixTree
   = RadixTree
   { _radixBloom :: RadixBloom
   , _radixBloomBits :: Int
   , _radixBuffer :: RadixBuffer
   , _radixCache :: RadixCache
   , _radixCheckpoint :: RadixRoot
   , _radixDatabase :: RadixDatabase
   , _radixRoot :: RadixRoot
   }
