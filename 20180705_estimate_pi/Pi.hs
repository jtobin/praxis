{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad.Primitive (PrimMonad, PrimState)
import System.Random.MWC as MWC
import qualified Control.Foldl as L
import Pipes ((>->))
import qualified Pipes as P
import qualified Pipes.Prelude as P

sample :: forall m. PrimMonad m => Gen (PrimState m) -> P.Producer Bool m ()
sample prng = do
    val <- P.lift (relPrime prng)
    P.yield val
    sample prng
  where
    relPrime rng =
      rprime <$> (uniform rng :: m Word) <*> (uniform rng :: m Word)

    rprime a b = gcd a b == 1

count :: Num a => L.Fold Bool a
count = L.Fold alg 0 id where
  alg acc tru
    | tru       = acc + 1
    | otherwise = acc

avg :: Fractional a => L.Fold Bool a
avg = (/) <$> count <*> L.genericLength

main :: IO ()
main = do
  prng <- createSystemRandom
  freq <- L.purely P.fold avg (sample prng >-> P.take 1000000)

  print (sqrt (6 / freq))

