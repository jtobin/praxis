import Test.QuickCheck
import Control.Monad

data MinStack a = Bottom | Stack a a (MinStack a)
    deriving (Eq, Read)

instance Show a => Show (MinStack a) where
    show Bottom        = "|"
    show (Stack m x s) =  "<" ++ show x ++ show s

push x s@(Stack m _ _) = if x < m then Stack x x s else Stack m x s
push x Bottom          = Stack x x Bottom

pop Bottom        = error "nothing there"
pop (Stack _ x s) = (x, s)

smin Bottom        = error "nothing there"
smin (Stack m _ _) = m

-- Testing

fromList :: Ord a => [a] -> MinStack a 
fromList = foldr push Bottom

toList :: MinStack a -> [a]
toList Bottom        = []
toList (Stack _ x s) = x : toList s

instance (Ord a, Arbitrary a) => Arbitrary (MinStack a) where
    arbitrary = liftM fromList (arbitrary :: Arbitrary a => Gen [a])

newtype NonEmptyMinStack a = NonEmptyMinStack {getValue :: MinStack a}
    deriving (Eq, Show, Read)

instance (Ord a, Arbitrary a) => Arbitrary (NonEmptyMinStack a) where
    arbitrary = liftM NonEmptyMinStack (arbitrary `suchThat` (/= Bottom))

sminReturnsMinimum (NonEmptyMinStack ms) = smin ms == minimum (toList ms)

newtype LargeMinStack a = LargeMinStack {getValueOfLargeMinStack :: MinStack a}
    deriving (Eq, Show, Read)

instance (Ord a, Arbitrary a) => Arbitrary (LargeMinStack a) where
    arbitrary = liftM LargeMinStack (arbitrary `suchThat` ((> 1) . length . toList))

sminUnaffectedByPopping (LargeMinStack ms) = let (x, s) = pop ms in
    if smin ms /= smin s then x == smin ms else True

