{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Codec.Serialise (deserialise, serialise)
import Data.ByteString as Strict (ByteString)
import Data.ByteString.Base16 (decode)
import Data.ByteString.Base16.Lazy (encode)
import Data.ByteString.Lazy as Lazy (ByteString)
import System.Exit (ExitCode(..), exitWith)
import Test.HUnit (Counts(..), Test(..), (@=?), runTestTT)

import Network.DFINITY.RadixTree.Types

main :: IO ()
main = do
   Counts {..} <- runTestTT tests
   exitWith $ case failures + errors of
      0 -> ExitSuccess
      _ -> ExitFailure 1

test :: RadixBranch -> Lazy.ByteString -> Test
test input result =
   TestList $ map TestCase [result @=? encode bytes, input @=? deserialise bytes]
   where bytes = serialise input

tests :: Test
tests = TestList
   [ test RadixBranch
      { _radixPrefix = Nothing
      , _radixLeft = Nothing
      , _radixRight = Nothing
      , _radixLeaf = Nothing
      } "83f6f6f6"
   , test RadixBranch
      { _radixPrefix = prefix 32 "74657374"
      , _radixLeft = Nothing
      , _radixRight = Nothing
      , _radixLeaf = value "636174"
      } "848218204474657374f6f643636174"
   , test RadixBranch
      { _radixPrefix = prefix 1 "00"
      , _radixLeft = value "d8297af08232c7b32a420280b31493c8f3e81c37"
      , _radixRight = value "cf98646d85611143c317072724ac5ed7f4b0052f"
      , _radixLeaf = Nothing
      } "8382014100d82a54d8297af08232c7b32a420280b31493c8f3e81c37d82a54cf98646d85611143c317072724ac5ed7f4b0052f"
   ]

prefix :: Int -> Strict.ByteString -> Maybe RadixPrefix
prefix n = Just . RadixPrefix n . fst . decode

value :: Strict.ByteString -> Maybe Strict.ByteString
value = Just . fst . decode
