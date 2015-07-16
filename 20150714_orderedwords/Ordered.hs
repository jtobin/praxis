
module Ordered where

import Data.Function (on)
import Data.List (sort, maximumBy)

longestOrdered :: Ord a => [[a]] -> Maybe [a]
longestOrdered dict = safeMaximumBy (compare `on` length)
  [word | word <- dict, sort word == word]

safeMaximumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
safeMaximumBy _ [] = Nothing
safeMaximumBy p xs = Just (maximumBy p xs)

