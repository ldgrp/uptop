module Main where

import Data.Aeson.Encode.Pretty
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client
import System.Environment

import qualified Data.ByteString.Lazy.Char8 as BLC

import Up

import Up.API
import Up.Model.Token


-- | Query the Up API
query :: ClientM a -> IO a
query q = do
    token <- Token <$> getEnv "UP_BANK_TOKEN"
    mgr <- newManager tlsManagerSettings
    res <- runClientM q (mkClientEnv mgr upBaseUrl) { makeClientRequest = makeUpClientRequest token}
    case res of
      Left err -> fail $ show err
      Right res' -> pure res'

main :: IO ()
main = do
    res <- query (listTransactions_ Nothing Nothing Nothing Nothing)
    BLC.putStrLn $ encodePretty res
