{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.List 
import Data.Hashable (Hashable)
import Data.HashMap.Strict hiding (filter)
import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.All
import Criterion.Main
import Criterion.Config
import System.Random.MWC 

-- O(n^2) ----------------------------------------------------------------------

-- nub, loop + elem are O(n^2)
i0 :: Eq a => [a] -> [a] -> [a]
i0 _  []     = []
i0 [] _      = []
i0 (x:xs) ys = nub $ if x `elem` ys then x : i0 xs ys else i0 xs ys 

-- nub is O(n^2), otherwise linear
u0 :: Eq a => [a] -> [a] -> [a]
u0 xs ys = nub $ xs ++ go xs ys
    where go as [] = as
          go [] bs = bs
          go as (b:bs) = if b `notElem` as then b : go as bs else go as bs

-- O(n log n) ------------------------------------------------------------------

-- sort (mergesort) is O(n log n), otherwise linear
i1 :: Ord a => [a] -> [a] -> [a]
i1 xs ys = go (sort xs) (sort ys) [] 
    where go [] _ cs = cs
          go _ [] cs = cs
          go al@(a:as) bl@(b:bs) cs = case compare a b of
                LT -> go as bl cs
                GT -> go al bs cs
                EQ -> go as bs (b:cs)

-- sort (mergesort) is O(n log n), otherwise linear
u1 :: Ord a => [a] -> [a] -> [a]
u1 xs ys = go (sort xs) (sort ys) []
    where go [] bs cs = bs ++ cs
          go as [] cs = as ++ cs
          go al@(a:as) bl@(b:bs) cs = case compare a b of
                LT -> go as bl (a:cs)
                GT -> go al bs (b:cs)
                EQ -> go as bs (a:cs)

-- O(n) ------------------------------------------------------------------------

-- filter, fromList, zip are O(n), otherwise constant
i2 :: (Enum a, Eq a, Hashable a) => [a] -> [a] -> [a]
i2 xs = filter (\y -> member y . fromList $ zip xs ([1..] :: [Int])) 

-- filter, fromList, zip are O(n), otherwise constant
u2 :: (Eq k, Hashable k) => [k] -> [k] -> [k]
u2 xs ys = xs ++ 
           filter (\y -> not . member y . fromList $ zip xs ([1..] :: [Int])) ys

-- Tests -----------------------------------------------------------------------

prop_i0ResultElementsAreInBoth :: Eq a => [a] -> [a] -> Bool
prop_i0ResultElementsAreInBoth xs ys = all (`elem` xs) zs && all (`elem` ys) zs
    where zs = i0 xs ys

prop_i1ResultElementsAreInBoth :: Ord a => [a] -> [a] -> Bool
prop_i1ResultElementsAreInBoth xs ys = all (`elem` xs) zs && all (`elem` ys) zs
    where zs = i1 xs ys

prop_i2ResultElementsAreInBoth :: (Enum a, Eq a, Num a, Hashable a) 
                               => [a] -> [a] -> Bool
prop_i2ResultElementsAreInBoth xs ys = all (`elem` xs) zs && all (`elem` ys) zs
    where zs = i2 xs ys

prop_u0ResultElementsInAtLeastOne :: Eq a => [a] -> [a] -> Bool
prop_u0ResultElementsInAtLeastOne xs ys = all (`elem` (xs ++ ys)) zs
    where zs = u0 xs ys

prop_u1ResultElementsInAtLeastOne :: Ord a => [a] -> [a] -> Bool
prop_u1ResultElementsInAtLeastOne xs ys = all (`elem` (xs ++ ys)) zs
    where zs = u1 xs ys

prop_u2ResultElementsInAtLeastOne :: (Eq a, Hashable a) => [a] -> [a] -> Bool
prop_u2ResultElementsInAtLeastOne xs ys = all (`elem` (xs ++ ys)) zs
    where zs = u2 xs ys

runTestSuite :: IO Bool
runTestSuite = $forAllProperties 
    (quickCheckWithResult (stdArgs {maxSuccess = 1000}))

-- Benchmarks ------------------------------------------------------------------

myConfig :: Config
myConfig = defaultConfig { cfgPerformGC = ljust True }

main :: IO ()
main = do
    prng       <- create
    [l0s, l1s] <- replicateM 2 $ replicateM 75  (uniformR (1 :: Int, 100) prng)
    [l0m, l1m] <- replicateM 2 $ replicateM 125 (uniformR (1 :: Int, 100) prng)
    [l0l, l1l] <- replicateM 2 $ replicateM 250 (uniformR (1 :: Int, 100) prng)

    void runTestSuite 

    defaultMain [
          bgroup "intersection - i0   " [ bench "i0s" $ nf (i0 l0s) l1s
                                        , bench "i0m" $ nf (i0 l0m) l1m
                                        , bench "i0l" $ nf (i0 l0l) l1l ]

        , bgroup "intersection - i1   " [ bench "i1s" $ nf (i1 l0s) l1s
                                        , bench "i1m" $ nf (i1 l0m) l1m
                                        , bench "i1l" $ nf (i1 l0l) l1l ]

        , bgroup "intersection - i2   " [ bench "i2s" $ nf (i2 l0s) l1s
                                        , bench "i2m" $ nf (i2 l0m) l1m
                                        , bench "i2l" $ nf (i2 l0l) l1l ]

        , bgroup "union - u0   " [ bench "u0s" $ nf (u0 l0s) l1s
                                 , bench "u0m" $ nf (u0 l0m) l1m
                                 , bench "u0l" $ nf (u0 l0l) l1l ]

        , bgroup "union - u1   " [ bench "u1s" $ nf (u1 l0s) l1s
                                 , bench "u1m" $ nf (u1 l0m) l1m
                                 , bench "u1l" $ nf (u1 l0l) l1l ]

        , bgroup "union - u2   " [ bench "u2s" $ nf (u2 l0s) l1s
                                 , bench "u2m" $ nf (u2 l0m) l1m
                                 , bench "u2l" $ nf (u2 l0l) l1l ]
        ]

