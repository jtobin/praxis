{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Currency (query) where

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Data.Monoid
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as Text
import Network.Wreq

apiUrl :: String
apiUrl = "http://www.freecurrencyconverterapi.com/api/v3/convert"

convertQuery :: String -> String -> String
convertQuery x y = apiUrl <> "?q=" <> x <> "_" <> y <> "&compact=y"

tp :: String -> Text
tp = Text.pack

query :: Scientific -> String -> String -> IO Scientific
query d x y = do
  r <- asJSON =<< get (convertQuery x y) :: IO (Response Value)
  let replyBody = do
        reply <- r ^? responseBody . key (tp x <> "_" <> tp y) . _Object
        val   <- reply ^. at "val"
        val ^? _Number

  case replyBody of
    Nothing -> error "invalid currency pair"
    Just v  -> return $ d * v

