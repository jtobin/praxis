{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Prelude hiding (Monad, return, (>>=))
import qualified Data.HashSet as HashSet
import Data.HashSet  (HashSet, fromList, empty, singleton)
import Data.Hashable (Hashable)

-- | Standard monad class with Hashable/Eq constraints on parameters.
class HashableMonad m where
    return :: Hashable a                     => a -> m a
    (>>=)  :: (Hashable a, Hashable b, Eq b) => m a -> (a -> m b) -> m b

instance HashableMonad HashSet where
    return  = singleton
    m >>= f = HashSet.foldr (HashSet.union . f) empty m

-- | Possible destinations of a move originaing from (c, r).
possibleMoves :: (Num a, Ord a, Hashable a) => (a, a) -> HashSet (a, a)
possibleMoves (c, r) = 
    if   (c < 0 || c > 7) || (r < 0 || r > 7)
    then error "possibleMoves: invalid origin"
    else let inBounds (x, y) = (x >= 0 && y >= 0) && (x <= 7 && y <= 7)
         in  fromList $ filter inBounds [ (c - 1, r - 2), (c - 1, r + 2) 
                                        , (c - 2, r - 1), (c - 2, r + 1)
                                        , (c + 1, r - 2), (c + 1, r + 2)
                                        , (c + 2, r - 1), (c + 2, r + 1) ]
{-# INLINE possibleMoves #-}
 
-- | The reachability of the destination from the origin in n steps.
canReachInN :: (Num a, Ord a, Hashable a) => (a, a) -> (a, a) -> a -> Bool
canReachInN (c0, r0) (c1, r1) = go (possibleMoves (c0, r0))
    where go !h !n | n <= 0    = False
                   | otherwise =  HashSet.member (c1, r1) h 
                               || go (h >>= possibleMoves) (n - 1)
{-# INLINE canReachInN #-}

-- | The number of moves required to reach the destination from the origin.
movesToReach :: (Num a, Ord a, Hashable a) => (a, a) -> (a, a) -> Integer
movesToReach a@(c0, r0) b@(c1, r1) 
    | (c1 < 0 || c1 > 7) || (r1 < 0 || r1 > 7) = 
      error "countMovesToReach: invalid destination"
    | a == b    = 0
    | otherwise = go (possibleMoves (c0, r0)) 1
  where go !h !n | HashSet.member b h = n
                 | otherwise          = go (h >>= possibleMoves) (n + 1)
{-# INLINE movesToReach #-}

-- | Print the number of moves required to move from (0, 0) to (7, 7).
printNumMovesToOppositeCorner :: IO ()
printNumMovesToOppositeCorner = print $ movesToReach (0 :: Int, 0 :: Int) 
                                                     (7 :: Int, 7 :: Int)

main :: IO ()
main = do putStrLn "Number of moves from (0, 0) to (7, 7)"
          printNumMovesToOppositeCorner

