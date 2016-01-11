-- First, find the first non-repeated element in an unsorted array. For
-- instance, in the array [3, 5, 3, 2, 1], element 3 is repeated, elements 5,
-- 2, and 1 are non-repeated, and the first of those is 5. Second, find the
-- first element that appears an even number of times in an unsorted array. For
-- instance, in the array [5, 3, 5, 1, 5, 1, 3], elements 1 and 3 appear an
-- even number of times, and the first of those is 3.

import Data.Function
import Data.List
import qualified Data.Map.Strict as MS
import qualified Data.Set as S

one :: Ord a => [a] -> a
one =
    snd
  . minimumBy (compare `on` fst)
  . concat
  . filter ((< 2) . length)
  . groupBy ((==) `on` snd)
  . sortBy (compare `on` snd) . zip [1..]

two :: Ord a => [a] -> a
two =
    snd
  . minimumBy (compare `on` fst)
  . concat
  . filter (even . length)
  . groupBy ((==) `on` snd)
  . sortBy (compare `on` snd) . zip [1..]

test0 = [3, 5, 3, 2, 1]
test1 = [5, 3, 5, 1, 5, 1, 3]


