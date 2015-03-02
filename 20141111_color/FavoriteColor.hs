{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Arrow
import Data.Function
import Data.List
import System.Environment

parse :: String -> String
parse input = fst $ maximumBy (compare `on` snd) colors where
  recs   = filter ((== "favoritecolor") . fst) . fmap record $ lines input
  colors = fmap ((head . nub) &&& length) . group . sort $ fmap snd recs

record :: String -> (String, String)
record rec = (key, val) where
  alphaNum = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
  key = takeWhile (/= ':') rec
  val = dropWhile (`notElem` alphaNum) . dropWhile (/= ':') $ rec

main :: IO ()
main = getArgs >>= \args -> case args of
  [] -> putStrLn "USAGE: ./favorite <FILE>"
  (file:_) -> readFile file >>= putStrLn . parse

