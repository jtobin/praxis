
module Matches where

appears :: Eq a => [a] -> [a] -> Int
appears = go where
  go [] _ = 1
  go _ [] = 0
  go ab@(b:bs) (c:cs)
    | b == c    = appears bs cs + appears ab cs
    | otherwise = appears ab cs

