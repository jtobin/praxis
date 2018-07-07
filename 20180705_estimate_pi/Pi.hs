{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}

import Control.Monad.Primitive (PrimMonad, PrimState)
import System.Random.MWC as MWC
import qualified Control.Foldl as L
import Pipes ((>->))
import qualified Pipes as P
import qualified Pipes.Prelude as P

sample
  :: PrimMonad m
  => Gen (PrimState m) -> P.Producer Bool m ()
sample prng = do
    val <- P.lift (rprime <$> uniform prng <*> uniform prng)
    P.yield val
    sample prng
  where
    rprime :: Word -> Word -> Bool
    rprime a b = gcd a b == 1

count :: Num a => L.Fold Bool a
count = L.Fold (\acc tr -> if tr then acc + 1 else acc) 0 id

avg :: Fractional a => L.Fold Bool a
avg = (/) <$> count <*> L.genericLength

main :: IO ()
main = do
  prng <- createSystemRandom
  freq <- L.purely P.fold avg (sample prng >-> P.take 1000000)

  print (sqrt (6 / freq))

