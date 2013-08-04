{-# OPTIONS_GHC -Wall #-}

-- Just working through the following for this one:
--
-- http://hamberg.no/erlend/posts/2013-08-02-ordered-hash-table.html

module HashTable where

import Data.Hashable
import qualified Data.List as L
import Data.Vector (Vector, (!), unsafeIndex)
import qualified Data.Vector as V
import Prelude hiding (lookup)

newtype HashTable a = HashTable (Vector [a]) deriving Show

-- Exported functions ----------------------------------------------------------

empty :: Int -> HashTable a
empty s = HashTable $ V.replicate s []

insert :: (Hashable a, Ord a) => HashTable a -> a -> HashTable a
insert (HashTable bs) e = HashTable $ V.update bs (V.singleton (pos, bucket))
  where pos    = position bs e
        bucket = L.insert e $ bs `unsafeIndex` pos

delete :: (Hashable a, Ord a) => HashTable a -> a -> HashTable a
delete (HashTable bs) e = HashTable $ V.update bs (V.singleton (pos, bucket))
  where pos    = position bs e
        bucket = L.delete e $ bs `unsafeIndex` pos

lookup :: (Hashable a, Ord a) => HashTable a -> a -> Maybe a
lookup (HashTable bs) e = L.find (== e) bucket
  where pos    = position bs e
        bucket = bs ! pos

-- Utilities -------------------------------------------------------------------

position :: Hashable a => Vector b -> a -> Int
position bs e = hash e `mod` V.length bs

