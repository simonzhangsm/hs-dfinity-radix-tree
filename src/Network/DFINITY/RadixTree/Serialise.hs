{-# LANGUAGE LambdaCase #-}

{-# OPTIONS -Wall #-}

module Network.DFINITY.RadixTree.Serialise
   ( decodeLeaf
   , decodeMaybe
   , decodeSide
   , encodeMaybe
   , encodeSide
   ) where

import Codec.Serialise (Serialise)
import Codec.Serialise.Decoding (Decoder, TokenType(..), decodeBytes, decodeNull, decodeTag, peekTokenType)
import Codec.Serialise.Encoding (Encoding, encodeBytes, encodeNull, encodeTag)
import Control.Monad (void)
import Data.ByteString.Char8 (ByteString)
import Data.Monoid ((<>))

decodeLeaf :: Int -> Decoder s (Maybe ByteString)
decodeLeaf = \ case
   3 -> pure Nothing
   4 -> pure <$> decodeBytes
   _ -> fail "decodeLeaf: invalid argument"

decodeMaybe :: Serialise a => Decoder s a -> Decoder s (Maybe a)
decodeMaybe value = peekTokenType >>= \ case
   TypeNull -> decodeNull >> pure Nothing
   _ -> pure <$> value

decodeSide :: Decoder s ByteString
decodeSide = void decodeTag >> decodeBytes

encodeMaybe :: Serialise a => (a -> Encoding) -> Maybe a -> Encoding
encodeMaybe = maybe encodeNull

encodeSide :: ByteString -> Encoding
encodeSide side = encodeTag 42 <> encodeBytes side
