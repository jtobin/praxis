import Control.Monad
import System.Random.MWC
import Test.QuickCheck

import Data.Map.Strict (empty, insert, deleteMax, findMax, size, toList)

distanceToNull :: Num a => (a, a) -> a
distanceToNull (x, y) = x^2 + y^2

smallestHundred :: (Num a, Ord a) => [(a, a)] -> [(a, a)]
smallestHundred xs = findSmallest xs empty where
    findSmallest []     m = map fst . toList $ m
    findSmallest (x:xs) m = case compare (size m) 100 of
          GT -> error "you done fucked up"
          LT -> findSmallest xs (insert x (distanceToNull x) m) 
          EQ -> if   distanceToNull x >= snd (findMax m)
                then findSmallest xs m
                else findSmallest xs (insert x (distanceToNull x) $ deleteMax m)
 
-- Tests -----------------------------------------------------------------------

prop_smallestHundredAreSmallest :: [(Double, Double)] -> Bool
prop_smallestHundredAreSmallest xs = 
    all (\x -> if x `notElem` sh then all (<= x) sh else True) xs
  where sh = smallestHundred xs

prop_smallestHundredAreMaxHundred :: [(Double, Double)] -> Bool
prop_smallestHundredAreMaxHundred xs = length sh <= 100
  where sh = smallestHundred xs

main = do
    -- QuickCheck stuff
    quickCheck prop_smallestHundredAreMaxHundred
    quickCheck prop_smallestHundredAreSmallest

    -- Unit test
    g   <- create
    z0s <- replicateM 1000000 (uniformR (-1000, 1000) g) :: IO [Int]
    z1s <- replicateM 1000000 (uniformR (-1000, 1000) g) :: IO [Int]
    
    print $ smallestHundred (zip z0s z1s)

