-- streaming entropy calculation; memory usage is linear in the number of
-- unique symbols

import           Control.Foldl            (Fold (..))
import qualified Control.Foldl            as L
import qualified Control.Foldl.ByteString as LB
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map
import           Data.Monoid              (mempty, (<>))
import           Pipes
import qualified Pipes.ByteString         as P
import qualified Pipes.Prelude            as PP

count :: (Ord k, Num a) => Fold k (Map k a)
count = Fold step mempty id where
  step m k
    | Map.member k m = Map.update (pure . (+ 1)) k m
    | otherwise      = Map.insert k 1 m

divide :: (Fractional b, Functor f) => f b -> b -> f b
divide m n = fmap (/ n) m

average :: (Ord k, Fractional a) => Fold k (Map k a)
average = divide <$> count <*> L.genericLength

-- in-memory entropy
entropy :: (Foldable f, Ord k, Floating a) => f k -> a
entropy xs = L.fold (L.premap (negate . entropize) L.sum) folded where
  folded   = L.fold average xs

entropize :: Floating a => a -> a
entropize m = m * logBase 2 m

-- streaming byte content-based entropy
main :: IO ()
main = do
  m <- case average of
    Fold step start f -> P.foldBytes step start f P.stdin
  print $ L.fold (L.premap (negate . entropize) L.sum) m

