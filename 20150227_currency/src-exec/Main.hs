
module Main where

import Currency
import Data.Scientific
import Options.Applicative
import Options.Applicative.Types

main :: IO ()
main = execParser parserOptions >>= runProgram

data ExchangeOptions = ExchangeOptions Scientific String String

-- | Options parser options.
parserOptions :: ParserInfo ExchangeOptions
parserOptions = info (helper <*> opts) fullDesc where
  opts =
        ExchangeOptions
    <$> argument parseSci (metavar "AMOUNT")
    <*> argument str (metavar "CURRENCY")
    <*> argument str (metavar "CURRENCY")

runProgram :: ExchangeOptions -> IO ()
runProgram (ExchangeOptions d x y) = do
  val <- query d x y
  putStrLn $ show d <> "@" <> x <> "/" <> y <> ": " <> show val

parseSci :: ReadM Scientific
parseSci = do
  sci <- readerAsk
  return $ read sci

