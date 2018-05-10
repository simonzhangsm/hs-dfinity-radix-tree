{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Network.DFINITY.RadixTree.Types
   ( RadixPrefix(..)
   , RadixBranch(..)
   , RadixTree(..)
   ) where

import Codec.Serialise (Serialise(..))
import Codec.Serialise.Decoding (decodeBytes, decodeInt, decodeListLen)
import Codec.Serialise.Encoding (encodeBytes, encodeInt, encodeListLen)
import Control.DeepSeq (NFData(..))
import Control.Monad (void)
import Data.Bool (bool)
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Short (ShortByteString)
import Data.Data (Data)
import Data.Default.Class (Default(..))
import Data.LruCache (LruCache)
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Database.LevelDB (DB)

import Network.DFINITY.RadixTree.Serialise

data RadixPrefix
   = RadixPrefix
   { _radixBitLen :: Int
   , _radixName :: ByteString
   } deriving (Data, Eq, Show)

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

data RadixBranch
   = RadixBranch
   { _radixPrefix :: Maybe RadixPrefix
   , _radixLeft :: Maybe ByteString
   , _radixRight :: Maybe ByteString
   , _radixLeaf :: Maybe ByteString
   } deriving (Data, Eq, Show)

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
      encodeMaybe encode _radixPrefix <>
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

data RadixTree
   = RadixTree
   { _radixCache :: LruCache ShortByteString ByteString
   , _radixDatabase :: DB
   , _radixRoot :: RadixBranch
   }
