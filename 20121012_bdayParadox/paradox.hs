{-# LANGUAGE BangPatterns, TemplateHaskell #-}

import System.Random.MWC
import Control.Arrow
import Control.Monad
import Data.List
import System.Exit
-- Testing
import System.Environment
import Test.QuickCheck
import Test.QuickCheck.All

simulate :: Int -> Int -> IO Double
simulate ne np = do
    zs <-   withSystemRandom . asGenIO $ \g0 -> replicateM ne 
          . fmap duplicatesPresent $ replicateM np (uniformR (1 :: Int, 365) g0) 
    return . average $ map (\x -> if x then 1 else 0) zs

main :: IO ()
main = do
    args <- getArgs
    when (args == []) $ putStrLn "(Birthday Paradox Simulator) usage: ./simulate <runTests=True|False>" >> exitSuccess
    let run = read $ head args :: Bool
    when   run (void runTests)
    unless run $ do let psim n ne np = do putStrLn $ "run " ++ show n ++ " (" ++ show np ++ " people, " ++ show ne ++ " simulations)"
                                          sresult <- simulate ne np
                                          putStrLn $ "    frequency of " ++ show sresult
                    psim 0 10000 12
                    psim 1 10000 23
                    psim 2 10000 57
                    psim 3 10000 100

-- Utilities 

average :: Fractional a => [a] -> a
average [] = error "average: empty list"
average xs = go 0 0 xs
  where go !s !n []     = s / fromIntegral n
        go !s !n (x:xs) = go (s + x) (n + 1) xs
{-# INLINE average #-}

duplicatesPresent :: (Eq a, Ord a) => [a] -> Bool
duplicatesPresent [] = False
duplicatesPresent xs = go h0 t0
    where go h []     = False
          go h (t:ts) = h == t || go t ts
          (h0, t0) = (head &&& tail) (sort xs)
{-# INLINE duplicatesPresent #-}

-- Testing

prop_averageEqualsNaiveMean :: NonEmptyList Double -> Bool
prop_averageEqualsNaiveMean (NonEmpty xs) = 
    average xs == (sum xs / fromIntegral (length xs))

prop_averageLiesBetweenExtrema :: NonEmptyList Double -> Bool
prop_averageLiesBetweenExtrema (NonEmpty xs) = 
    (minimum xs <= average xs) && (maximum xs >= average xs)

prop_duplicatesPresentDetectsDuplicates :: [Int] -> Bool
prop_duplicatesPresentDetectsDuplicates xs = 
    duplicatesPresent xs == (any (> 1) . map length . group . sort) xs
                      
qcOptions :: Args
qcOptions = stdArgs { maxSuccess = 1000 } 

runTests :: IO Bool
runTests = $forAllProperties (quickCheckWithResult qcOptions)

