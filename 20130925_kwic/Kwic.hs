-- The KWIC index system accepts an ordered set of lines, each line is an
-- ordered set of words, and each word is an ordered set of characters. Any
-- line may be "circularly shifted" by repeatedly removing the first word and
-- appending it at the end of the line. The KWIC index system outputs a listing
-- of all circular shifts of all lines in alphabetical order. This is a small
-- system. Except under extreme circumstances (huge data base, no supporting
-- software), such a system could be produced by a good programmer within a
-- week or two.
--
-- (24 minutes in 2013)
-- - j

import Control.Monad
import Data.List
import System.Environment
import System.Exit

main :: IO ()
main = do
  args    <- getArgs
  file    <- parseArgs args
  content <- fmap lines (readFile file)
  mapM_ putStrLn $ kwicSystem content

parseArgs :: [a] -> IO a
parseArgs []    = putStrLn "USAGE: ./kwic <filename>" >> exitSuccess
parseArgs (x:_) = return x

kwicSystem :: [String] -> [String]
kwicSystem = map unwords
           . sort 
           . concatMap (allRotations . words) 

rotateByOneWord :: [a] -> [a]
rotateByOneWord ws = take (length ws) . drop 1 . cycle $ ws

allRotations :: [a] -> [[a]]
allRotations ws = go (length ws - 1) [ws]
  where go 0 acc = init acc
        go j acc = go (pred j) (rotateByOneWord (head acc) : acc)

