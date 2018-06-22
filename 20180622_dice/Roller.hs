{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Monad (replicateM)
import qualified Data.Char as C (digitToInt, isDigit)
import qualified Data.List as L (foldl', unfoldr)
import Data.Monoid ((<>))
import qualified Data.Random as R
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Foldable as F (for_)

data Roll = Roll {
    ndie :: !Int
  , nsid :: !Int
  } deriving Show

newtype Rolls = Rolls {
    rolls :: [Int]
  } deriving Show

parseLine :: T.Text -> Maybe Roll
parseLine (T.strip -> input) = case T.splitOn "d" input of
  [nd, ns] -> do
    ndie <- parseInt nd
    nsid <- parseInt ns

    if   (ndie > 0 && ndie < 101) && (nsid > 1 && nsid < 101)
    then return Roll {..}
    else Nothing

  _        -> Nothing

parseInt :: T.Text -> Maybe Int
parseInt input
    | T.any (not . C.isDigit) input = Nothing
    | otherwise                     = Just parsed
  where
    lints        = T.foldl' alg [] input
    alg acc char = C.digitToInt char : acc

    ptens   = L.unfoldr coalg (0 :: Int)
    coalg j = Just (10 ^ j, succ j)

    parsed = L.foldl' (+) 0 (zipWith (*) lints ptens)

eval :: Roll -> IO Rolls
eval Roll {..} = do
  rolls <- replicateM ndie (R.sample (R.uniform 1 nsid))
  return Rolls {..}

render :: Show a => a -> T.Text
render = T.pack . show

present :: Rolls -> T.Text
present Rolls {..} = render (sum rolls) <> " -- " <> render rolls

main :: IO ()
main = do
  input <- TIO.getContents

  let feed = T.lines input
      rs   = fmap parseLine feed

  F.for_ rs $ \result ->
    case result of
      Nothing -> do
        TIO.putStrLn "usage: ./roll MdN"
        TIO.putStrLn "(M in 1 -- 100, N in 2 -- 100)"

      Just roll -> do
        results <- eval roll
        TIO.putStrLn (present results)

