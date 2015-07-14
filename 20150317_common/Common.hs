
module Common where

import Control.Monad
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

commons :: Ord a => [[a]] -> [a]
commons = expand . intersection . fmap sparseRepr

sparseRepr :: Ord a => [a] -> Map a Int
sparseRepr = Map.fromList . fmap labelAndCount . group where
  labelAndCount v = (head v, length v)

intersection :: (Ord k, Ord v) => [Map k v] -> Map k v
intersection [] = Map.empty
intersection (m:ms) = foldl' (Map.intersectionWith min) m ms

expand :: Map a Int -> [a]
expand = uncurry (flip replicate) <=< Map.toList

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
