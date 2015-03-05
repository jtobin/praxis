
module Power where

import Data.Foldable
import Data.Monoid

powLinear :: Num c => Product c -> Int -> c
powLinear a b = getProduct . fold $ replicate b a

powLogarithmic :: (Num b, Integral a) => b -> a -> b
powLogarithmic a b
  | b <  0 = error "nonnegative powers only"
  | b == 0 = 1
  | even b =
      let term = powLogarithmic a (b `quot` 2)
      in  term * term
  | otherwise =
      let term = powLogarithmic a (pred b `quot` 2)
      in  a * term * term

powConstant :: Floating a => a -> a -> a
powConstant a b = exp (log a * b)

