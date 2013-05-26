{-# OPTIONS_GHC -Wall #-}

import Data.Function
import Data.List

main :: IO ()
main = print $ minimumBy (compare `on` sumDiffs) portfolios

-- Data ------------------------------------------------------------------------

-- | A list of stocks we're interested in.
stocks :: [String]
stocks = [ "HUSK",  "NALC", "SUNC", "EXXN"
         , "AAPL",  "MSFT", "CISC", "ORAC"
         ,  "BHP",  "GOLD",  "JPM",  "BOA"
         ,   "GM",  "FORD", "VOLK",  "KIA"
         , "CHAR", "MERRY", "KENM", "TOPS"
         ]

-- | A list of sectors to match.
sectors :: [String]
sectors = concatMap (replicate 4) 
            [ "Energy", "Technology", "Finance", "Auto", "STJOHNS"]

-- | Market betas for each.
betas :: [Float]
betas = [  0.56, -0.76,  0.81, 0.34
        ,  0.79,  0.88,  0.98, 0.65
        ,  0.38, -0.89,  0.91, 0.32
        , -0.05, -0.23,  0.16, 0.65
        ,  0.53,  0.28, -0.06, 0.72
        ] 

-- | All combinations of unique stock pairs such that the first stock is from 
--   the Energy sector.
combos :: [((String, String), String, Float)]
combos = let paired = zip3 stocks sectors betas
         in  [ ((a, d), e, abs (c - f)) | (a, b, c) <- paired
                                        , (d, e, f) <- paired
                                        , b /= e, b == "Energy" ]

-- | All possible portfolios.
portfolios :: [Portfolio]
portfolios = [ (huskPair, nalcPair, suncPair, exxnPair)
             |   huskPair <- pickStock "HUSK" combos
               , nalcPair <- pickStock "NALC" combos
               , suncPair <- pickStock "SUNC" combos
               , exxnPair <- pickStock "EXXN" combos
               , second huskPair /= second nalcPair
               , second huskPair /= second suncPair
               , second huskPair /= second exxnPair
               , second nalcPair /= second suncPair
               , second nalcPair /= second exxnPair
               , second suncPair /= second exxnPair
             ] 

-- Utilities -------------------------------------------------------------------

-- | The sum of beta differences for a portfolio.
sumDiffs :: Portfolio -> Float
sumDiffs (h, n, s, e) = third h + third n + third s + third e

-- | Get the first element of a triple.
first ::  (t, t1, t2) -> t
first  (a, _, _) = a

-- | Get the second element of a triple.
second ::  (t, t1, t2) -> t1
second (_, b, _) = b

-- | Get the third element of a triple.
third ::  (t, t1, t2) -> t2
third  (_, _, c) = c

-- | Retrieve combinations of stock pairs including the given stock.
pickStock :: Eq a => a -> [((a, b), t, t1)] -> [((a, b), t, t1)]
pickStock  s = filter (\x -> (fst . first)  x == s)

-- | Just an abbreviation for the Portfolio type.
type Portfolio = (((String, String), String, Float)
                 ,((String, String), String, Float)
                 ,((String, String), String, Float)
                 ,((String, String), String, Float))

