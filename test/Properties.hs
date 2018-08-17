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
module Properties (tests) where

import qualified Data.Map as M
import Data.ByteString.Char8 (ByteString, pack)
import Data.ByteString.Short (fromShort)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck
import Data.List
import Data.Bifunctor
import Control.Monad.State.Strict

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

arbBS :: Gen ByteString
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

prop_lookup :: Property
prop_lookup  = forAll generateOps $ \ops ->
    runPure ops === runRadix ops

{-
We want to test if two different ways of generating the same map yield the same
state root. We first generate one final state (a Data.Map). Then we generate two
different set of operations that should yield that state. Then we run them and
compare the output.
-}

genMap :: Gen (M.Map ByteString ByteString)
genMap = M.fromList . map (bimap pack pack) . M.toList <$> arbitrary

genOpsForMap :: M.Map ByteString ByteString -> Gen [Op]
genOpsForMap m = do
    inserts <- forM (M.toList m) $ \(k,v) -> do
        ops <- genOpsForKey k
        return $ ops ++ [ Insert k v ]
    -- Extra keys (which we delete as the last action)
    extra_keys <- listOf $ arbBS `suchThat` (`M.notMember` m)
    deletes <- forM extra_keys $ \k -> do
        ops <- genOpsForKey k
        return $ ops ++ [ Delete k ]
    interleaves (inserts ++ deletes)
  where
    genOpsForKey k = listOf (oneof [ Insert k <$> arbBS , pure (Delete k) ])


-- TODO
interleaves :: [[a]] -> Gen [a]
interleaves xss = frequency'
    [ (length (x:xs), (x:) <$> interleaves (xs : xss))
    | ((x:xs):xss) <- rots xss ]
 where
   frequency' [] = return []
   frequency'  xs = frequency xs
   rots xs = tail $ zipWith (++) (tails xs) (inits xs)


type TreeGenOps = [ (ByteString, [Maybe ByteString]) ]

prop_stateRoot :: Property
prop_stateRoot = forAll genMap $ \m ->
    forAll (genOpsForMap m) $ \ops1 ->
    forAll (genOpsForMap m) $ \ops2 ->
    run ops1 === run ops2
  where
    run :: [Op] -> ByteString
    run ops0 = evalState (initTree >>= go ops0) M.empty
      where
        initTree :: M (RadixTree ())
        initTree = createRadixTree 262144 2028 Nothing ()

        go :: [Op] -> RadixTree () -> M ByteString
        go []                 t = fromShort . fst <$> merkleizeRadixTree t
        go (Insert k v : ops) t = insertRadixTree k v t >>= go ops
        go (Delete k   : ops) t = deleteRadixTree k t   >>= go ops
        go (Lookup _   : _  ) _ = error "no lookup in this test please"

tests :: TestTree
tests = testGroup "Property tests"
    [ testProperty "lookup" prop_lookup
    ]
