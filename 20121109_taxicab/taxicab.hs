-- Spec: write a function that returns all of the ways a number can be written 
-- as the sum of two non-negative cubes.

{-# OPTIONS_GHC -Wall -Werror #-}

import Control.Arrow
import Test.QuickCheck 

cubeDecomposition :: Int -> [(Int, Int)]
cubeDecomposition n = 
    [(x, y) | x <- [1..m], y <- [x..m], x^(3 :: Int) + y^(3 :: Int) == n] 
  where m = truncate $ fromIntegral n ** (1/3 :: Double)

main :: IO ()
main = do
    let highConfidence = stdArgs {maxSuccess = 10000}
    putStrLn "running tests.." 
    quickCheckWith highConfidence (forAll smallInts cubedElementsSumToN)

-- Tests -----------------------------------------------------------------------

smallInts :: Gen Int
smallInts = choose (-100000000, 100000000)

cubedElementsSumToN :: Int -> Bool
cubedElementsSumToN n = all (== n) d 
    where d = map (uncurry (+) . ((^(3 :: Int)) *** (^(3 :: Int)))) 
                    (cubeDecomposition n)

