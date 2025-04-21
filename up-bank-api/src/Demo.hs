module Main where

import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BLC
import Servant.Client
import System.Environment
import Up
import Up.API
import Up.Model.Token

-- | Query the Up API
query :: ClientM a -> IO a
query q = do
  token <- Token <$> getEnv "UP_BANK_TOKEN"
  upClient <- mkUpClient token
  res <- runClientM q upClient
  case res of
    Left err -> fail $ show err
    Right res' -> pure res'

main :: IO ()
main = do
  putStrLn "Listing accounts..."
  res <- query (listAccounts_ Nothing)
  BLC.putStrLn $ encodePretty res
