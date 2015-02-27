
module ForwardDifference where

leastForwardDifference :: (Ord a, Num a) => [a] -> Maybe a
leastForwardDifference xs = safeMaximum diffs where
  diffs = zipWith (-) xs mins
  mins  = scanl1 min xs

safeMaximum :: Ord a => [a] -> Maybe a
safeMaximum []     = Nothing
safeMaximum (x:xs) = Just $ foldr max x xs

