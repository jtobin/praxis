import Data.Foldable (toList)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Test.QuickCheck

swap :: Int -> [a] -> [a]
swap k xs = toList xsSwap
  where 
    xsAsSeq = Seq.fromList xs
    n       = Seq.length xsAsSeq
    xsSwap  = let kth   = xsAsSeq `Seq.index` k
                  nlkth = xsAsSeq `Seq.index` (n - k - 1)
              in  Seq.update (n - k - 1) kth . Seq.update k nlkth $ xsAsSeq

-- Tests -----------------------------------------------------------------------

newtype Constrained = Constrained { getItems :: (Int, [Int]) } deriving Show

instance Arbitrary Constrained where
  arbitrary = do
    NonEmpty xs <- arbitrary :: Gen (NonEmptyList Int)
    let n = length xs
    k <- choose (0, n - 1)
    return $ Constrained (k, xs)

prop_swapsKth :: Constrained -> Bool
prop_swapsKth (Constrained (k, xs)) = 
  let (swapped, n) = (swap k xs, length xs)
  in  xs !! k == swapped !! (n - k - 1)
   && xs !! (n - k - 1) == swapped !! k

