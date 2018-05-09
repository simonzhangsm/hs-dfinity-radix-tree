{-# LANGUAGE RecordWildCards #-}

module Network.DFINITY.RadixTree.Types
   ( RadixPrefix(..)
   , RadixBranch(..)
   ) where

import Codec.Serialise (Serialise(..))
import Codec.Serialise.Decoding (Decoder, TokenType(..), decodeBytes, decodeInt, decodeListLen, decodeNull, decodeTag, peekTokenType)
import Codec.Serialise.Encoding (Encoding, encodeBytes, encodeInt, encodeListLen, encodeNull, encodeTag)
import Control.Monad (void)
import Data.Bool (bool)
import Data.ByteString.Char8 (ByteString)
import Data.Maybe (isJust)
import Data.Monoid ((<>))

data RadixPrefix
   = RadixPrefix
   { _radixBitLen :: Int
   , _radixName :: ByteString
   } deriving (Eq, Show)

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
   } deriving (Eq, Show)

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

encodeMaybe :: Serialise a => (a -> Encoding) -> Maybe a -> Encoding
encodeMaybe = maybe encodeNull

encodeSide :: ByteString -> Encoding
encodeSide side = encodeTag 42 <> encodeBytes side

decodeMaybe :: Serialise a => Decoder s a -> Decoder s (Maybe a)
decodeMaybe value = do
   token <- peekTokenType
   case token of
      TypeNull -> decodeNull >> pure Nothing
      _ -> Just <$> value

decodeSide :: Decoder s ByteString
decodeSide = void decodeTag >> decodeBytes

decodeLeaf :: Int -> Decoder s (Maybe ByteString)
decodeLeaf len =
   case len of
      3 -> pure Nothing
      4 -> Just <$> decodeBytes
      _ -> fail "decodeLeaf: invalid argument"
