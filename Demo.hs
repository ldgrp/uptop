module Demo where

import Data.Aeson.Encode.Pretty
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client
import System.Environment

import qualified Data.ByteString.Lazy.Char8 as BLC

import Up

import Up.API
import Up.Model.Category
import Up.Model.Account
import Up.Model.Paginated
import Up.Model.Transaction
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

run :: IO ()
run = do
    res <- query (listTransactions_ Nothing Nothing Nothing Nothing)
    BLC.putStrLn $ encodePretty res
      
main :: IO ()
main = run
