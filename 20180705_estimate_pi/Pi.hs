{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}

import qualified Control.Foldl as L
import Control.Monad.Primitive
import Pipes ((>->))
import qualified Pipes as P
import qualified Pipes.Prelude as P
import System.Random.MWC as MWC

sample
  :: PrimMonad m
  => Gen (PrimState m)
  -> P.Producer Bool m ()
sample prng = do
    m <- P.lift (uniform prng)
    n <- P.lift (uniform prng)
    P.yield (rprime m n)
    sample prng
  where
    rprime :: Word -> Word -> Bool
    rprime a b = gcd a b == 1

count :: Num a => L.Fold Bool a
count = L.Fold alg 0 id where
  alg acc tr
    | tr = acc + 1
    | otherwise = acc

avg :: Fractional a => L.Fold Bool a
avg = (/) <$> count <*> L.genericLength

main :: IO ()
main = do
  prng <- createSystemRandom

  let pipe = sample prng >-> P.take (10 ^ 6)
  freq <- L.purely P.fold avg pipe

  print (sqrt (6 / freq))

