import Control.Arrow
import Control.Monad
import Control.Monad.Primitive
import Control.Parallel.Strategies
import System.Random.MWC

select :: PrimMonad m => Int -> [Int] -> Gen (PrimState m) -> m Int
select _ [x] _ = return x
select k xs  g = do
    pivot <- liftM (xs !!) (uniformR (0, length xs - 1) g)

    let (lesser, greater) = (filter (< pivot) &&& filter (> pivot)) xs `using` parTuple2 rseq rseq
        nl                = length lesser

    if   nl > k
    then select k (tail lesser) g
    else select (k - nl) greater g

main = do
    g  <- create
    xs <- replicateM 10000 (uniformR (-100 :: Int, 4000) g)

    k <- uniformR (1 :: Int, 10000) g

    z <- select k xs g
    print z

