{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

import           Control.Foldl    (Fold (..))
import qualified Control.Foldl    as L
import           Data.Map.Strict  (Map)
import qualified Data.Map.Strict  as Map
import           Pipes            ((>->))
import qualified Pipes.ByteString as P

group :: (Ord k, Num a) => Fold k (Map k a)
group = Fold step mempty id where
  step m k = Map.insertWith (+) k 1 m

distribution :: (Ord k, Fractional a) => Fold k (Map k a)
distribution = divide <$> group <*> L.genericLength where
  divide m n = fmap (/ n) m

entropize :: Floating a => a -> a
entropize m = negate (m * logBase 2 m)

entropy :: (Foldable f, Ord k, Floating a) => f k -> a
entropy = L.fold (L.premap entropize L.sum) . L.fold distribution

main :: IO ()
main = do
  let source = P.stdin >-> P.filter (/= 10)
  dist <- case distribution of
    Fold step start extract -> P.foldBytes step start extract source
  print $ L.fold L.sum (fmap entropize dist)

