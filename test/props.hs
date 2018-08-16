{-# LANGUAGE LambdaCase #-}

{- |
This module contains a (very simple) property based test for the RadixTree data
structure.

It uses QuickCheck to generate a list of operations (insert, delete, lookup),
executes them both against
 * "Data.Map" as a known-to-be-good specification and
 * RadixTree (with "Data.Map" as the backend)
records the resulting values in each case and compares these lists.

This is very rudimentary. In particular, 'generateOps' can be further tweaked
to produce “interesting” sequences -- there is little (but some) value in
deleting mostly entries that are not there.

You can sample what 'generateOps' produces by opening this file in ghci
(e.g. cabal new-repl property-tests) and then running @sample generateOps@.

-}
module Main where

import qualified Data.Map as M
import Data.ByteString.Char8 (ByteString, pack)
import Test.QuickCheck
import Data.List
import Control.Monad.State.Strict
import System.Exit

import Network.DFINITY.RadixTree

{-
In this module we test the RadixTree module against a pure implementation of
a map (Data.Map). We generate random sequences of Insert, Delete and Lookup
calls, run them in both implementations, and compare the results.
 -}

data Op
   = Insert ByteString ByteString
   | Delete ByteString
   | Lookup ByteString
  deriving Show

generateOps :: Gen [Op]
generateOps = do
    -- Create a small, non-zero number of keys to consider
    len <- getSmall . getPositive <$> arbitrary
    keys <- vectorOf len arbBS
    let pickKey = elements keys
    listOf $ frequency
        [ (1, Insert <$> pickKey <*> arbBS )
        , (1, Delete <$> pickKey)
        , (2, Lookup <$> pickKey)
        ]
  where
    arbBS = pack <$> arbitrary


runPure :: [Op] -> [Maybe ByteString]
runPure ops = snd $ mapAccumL go M.empty ops
  where
    go m (Insert k v) = (M.insert k v m, Nothing)
    go m (Delete k)   = (M.delete k m,   Nothing)
    go m (Lookup k)   = (m,              M.lookup k m)


type M = State (M.Map ByteString ByteString)

runRadix :: [Op] -> [Maybe ByteString]
runRadix ops0 = evalState (initTree >>= go ops0) M.empty
  where
    initTree :: M (RadixTree ())
    initTree = createRadixTree 262144 2028 Nothing ()

    go :: [Op] -> RadixTree () -> M [Maybe ByteString]
    go []                 _ = return []
    go (Insert k v : ops) t = do
        t' <- insertRadixTree k v t
        (Nothing :) <$> go ops t'
    go (Delete k   : ops) t = do
        t' <- deleteRadixTree k t
        (Nothing :) <$> go ops t'
    go (Lookup k   : ops) t =
        lookupRadixTree k t >>= \case
            Nothing     -> (Nothing :) <$> go ops t
            Just (v,t') -> (Just v :)  <$> go ops t'

main_prop :: Property
main_prop = forAll generateOps $ \ops ->
    runPure ops === runRadix ops

main :: IO ()
main = quickCheckResult main_prop >>= \case
    Success {} -> exitSuccess
    _          -> exitFailure
