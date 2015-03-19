{-# LANGUAGE FlexibleInstances #-}

module Common where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

commons :: (Eq a, Ord a) => [[a]] -> [a]
commons = expand . intersections . fmap toMap

toMap :: (Eq a, Ord a) => [a] -> Map a Int
toMap = Map.fromList . fmap labelAndCount . group where
  labelAndCount v = (head $ nub v, length v)

intersections :: (Ord k, Ord v) => [Map k v] -> Map k v
intersections [] = Map.empty
intersections (m:ms) = foldl' (Map.intersectionWith min) m ms

expand :: Map a Int -> [a]
expand = concatMap (uncurry (flip replicate)) . Map.toList

test0 = [1,5,10,20,40,80]
test1 = [6,7,10,20,80,100]
test2 = [3,4,15,20,30,70,80,120]

test3 = [1,5,5,5]
test4 = [3,4,5,5,10]
test5 = [5,5,10,20]

main :: IO ()
main = do
  print $ commons [test0, test1, test2]
  print $ commons [test3, test4, test5]

