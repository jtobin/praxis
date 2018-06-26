{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}

import Data.Char (isLower, toLower)
import Data.List as L (foldl', sortBy)
import Data.Function (on)
import Data.Monoid (mempty)
import qualified Data.Map.Strict as MS

tally :: String -> [(Char, Int)]
tally = sortBy (flip compare `on` snd) . MS.toList . accum

accum :: String -> MS.Map Char Int
accum = L.foldl' alg mempty where
  alg !acc score =
    let adjuster val = case (val, isLower score) of
          (Nothing, True)  -> Just 1
          (Nothing, False) -> Just (negate 1)
          (Just s, True)   -> Just (succ s)
          (Just s, False)  -> Just (pred s)

    in  MS.alter adjuster (toLower score) acc

test :: String
test = "EbAAdbBEaBaaBBdAccbeebaec"

main :: IO ()
main = print (tally test)

