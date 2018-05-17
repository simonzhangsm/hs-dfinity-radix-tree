{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Network.DFINITY.RadixTree.Types
   ( RadixBranch(..)
   , RadixCache
   , RadixPrefix(..)
   , RadixTree(..)
   ) where

import Codec.Serialise as CBOR (Serialise(..), serialise)
import Codec.Serialise.Decoding (decodeBytes, decodeInt, decodeListLen)
import Codec.Serialise.Encoding (encodeBytes, encodeInt, encodeListLen)
import Crypto.Hash.SHA256 (hash)
import Control.DeepSeq (NFData(..))
import Control.Monad (void)
import Data.Bool (bool)
import Data.ByteString.Base16 as Base16 (encode)
import Data.ByteString.Char8 (ByteString, unpack)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Short (ShortByteString)
import Data.Data (Data)
import Data.Default.Class (Default(..))
import Data.LruCache (LruCache)
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Database.LevelDB (DB)
import Text.Printf (printf)

import Network.DFINITY.RadixTree.Bits
import Network.DFINITY.RadixTree.Serialise

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

data RadixBranch
   = RadixBranch
   { _radixPrefix :: Maybe RadixPrefix
   , _radixLeft :: Maybe ByteString
   , _radixRight :: Maybe ByteString
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
      encodeMaybe encodeSide _radixLeft <>
      encodeMaybe encodeSide _radixRight <>
      maybe mempty encodeBytes _radixLeaf
      where len = bool 3 4 $ isJust _radixLeaf
   decode = do
      len <- decodeListLen
      prefix <- decodeMaybe decode
      left <- decodeMaybe decodeSide
      right <- decodeMaybe decodeSide
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
      left = color 4 $ maybe "null" format _radixLeft
      right = color 4 $ maybe "null" format _radixRight

type RadixCache = LruCache ShortByteString ByteString

data RadixTree
   = RadixTree
   { _radixCache :: RadixCache
   , _radixDatabase :: DB
   , _radixRoot :: ByteString
   }
