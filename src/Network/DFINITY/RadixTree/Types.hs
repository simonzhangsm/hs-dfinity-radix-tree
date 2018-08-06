{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS -Wall #-}

module Network.DFINITY.RadixTree.Types
   ( RadixBloom
   , RadixBranch
   , RadixBuffer
   , RadixCache
   , RadixDatabase(..)
   , RadixError(..)
   , RadixNode(..)
   , RadixPrefix(..)
   , RadixRoot
   , RadixSearchResult
   , RadixTree(..)
   ) where

import Codec.Serialise as CBOR (Serialise(..), serialise)
import Codec.Serialise.Decoding (decodeBytes, decodeInt, decodeListLen)
import Codec.Serialise.Encoding (encodeBytes, encodeInt, encodeListLen)
import Control.DeepSeq (NFData(..))
import Control.Exception (Exception)
import Control.Monad (void)
import Control.Monad.IO.Class
import Control.Monad.State.Strict as State (StateT, get, modify)
import Crypto.Hash.SHA256 (hash)
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
import Data.Map.Strict as Map (Map, insert, lookup)
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Database.LevelDB as LevelDB (DB, defaultReadOptions, defaultWriteOptions, get, put)
import Text.Printf (printf)

import Network.DFINITY.RadixTree.Bits
import Network.DFINITY.RadixTree.Serialise

type RadixBloom = Bloom RadixRoot

type RadixBranch = [RadixNode]

type RadixBuffer = Map RadixRoot RadixNode

type RadixCache = LruCache RadixRoot RadixNode

class Monad m => RadixDatabase m database where
   load :: database -> ByteString -> m (Maybe ByteString)
   store :: database -> ByteString -> ByteString -> m ()

instance Monad m => RadixDatabase (StateT (Map ByteString ByteString) m) () where
   load _ key = Map.lookup key <$> State.get
   store _ key = modify . insert key

instance MonadIO m => RadixDatabase m DB where
   load database = LevelDB.get database defaultReadOptions
   store database = put database defaultWriteOptions

data RadixError
   = InvalidArgument String
   | StateRootDoesNotExist RadixRoot
     deriving (Data, Eq, Show)

instance Exception RadixError

data RadixNode
   = RadixNode
   { _radixPrefix :: Maybe RadixPrefix
   , _radixLeft :: Maybe RadixRoot
   , _radixRight :: Maybe RadixRoot
   , _radixLeaf :: Maybe ByteString
   } deriving (Data, Eq)

instance NFData RadixNode where
   rnf RadixNode {..} =
      rnf _radixPrefix `seq`
      rnf _radixLeft `seq`
      rnf _radixRight `seq`
      rnf _radixLeaf `seq`
      ()

instance Default RadixNode where
   def = RadixNode Nothing Nothing Nothing Nothing

instance Serialise RadixNode where
   encode RadixNode {..} =
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
      pure $ RadixNode prefix left right leaf

instance Show RadixNode where
   show node@RadixNode {..} =
      case unpack <$> _radixLeaf of
         Nothing -> printf "\ESC[96m%s\ESC[0m@[\ESC[97m%s\ESC[0m,\ESC[96m%s\ESC[0m,\ESC[96m%s\ESC[0m]" root prefix left right
         Just leaf -> printf "\ESC[96m%s\ESC[0m@[\ESC[97m%s\ESC[0m,\ESC[96m%s\ESC[0m,\ESC[96m%s\ESC[0m,\ESC[97m%s\ESC[0m]" root prefix left right leaf
      where
      root = format $ hash $ toStrict $ serialise node
      prefix = guard show _radixPrefix
      left = guard format $ fromShort <$> _radixLeft
      right = guard format $ fromShort <$> _radixRight
      guard = maybe "null"
      format = take 8 . unpack . Base16.encode

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

type RadixSearchResult = (NonEmpty RadixRoot, NonEmpty RadixNode, NonEmpty [Bool], [Bool], [Bool], RadixCache)

data RadixTree database
   = RadixTree
   { _radixBloom :: RadixBloom
   , _radixBloomSize :: Int
   , _radixBuffer :: RadixBuffer
   , _radixCache :: RadixCache
   , _radixCacheSize :: Int
   , _radixCheckpoint :: RadixRoot
   , _radixDatabase :: database
   , _radixNonce :: Word
   , _radixRoot :: RadixRoot
   }
