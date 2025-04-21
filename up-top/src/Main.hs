{-# LANGUAGE OverloadedStrings #-}

module Main where

import App
import Auth
import Brick.BChan
import Brick.Main
import qualified Brick.Widgets.List as L
import Control.Concurrent
import Control.Monad
import Data.HashMap.Strict
import qualified Data.Text as T
import qualified Data.Vector as Vec
import Graphics.Vty
import Graphics.Vty.CrossPlatform
import Servant.Client
import System.Environment
import System.Exit
import Types
import Up
import Up.API
import Up.Model.Category
import Up.Model.Token

main :: IO ()
main = do
  let buildVty = mkVty defaultConfig
  initialVty <- buildVty

  -- Read the token environment variable
  envToken <- lookupEnv "UP_BANK_TOKEN"

  (authEvent, vty) <- interactiveAuth initialVty buildVty envToken

  -- Either we have a working token, or the user has exited early.
  token <- case authEvent of
    Just (ASuccess (AuthInfo tok)) -> pure $ T.unpack tok
    Just _ae -> do
      shutdown vty
      exitSuccess
    Nothing -> do
      shutdown vty
      exitSuccess

  env <- mkUpClient (Token token)

  requestChan <- newBChan 100
  responseChan <- newBChan 100

  void $ forkIO $ requestWorker env requestChan responseChan
  writeBChan requestChan FetchAccounts
  writeBChan requestChan FetchCategories
  void $ customMainWithVty vty buildVty (Just responseChan) app (initialState env requestChan)
  shutdown vty

-- Thread for handling requests
requestWorker :: ClientEnv -> BChan URequest -> BChan UEvent -> IO ()
requestWorker env requestChan responseChan = forever $ do
  req <- readBChan requestChan
  case req of
    FetchTransaction aid -> do
      res <- query (listTransactionsByAccount_ aid Nothing Nothing Nothing Nothing)
      case res of
        Right ts -> writeBChan responseChan $ UTransactions (aid, ts)
        Left err -> writeBChan responseChan $ UError (show err)
    FetchAccount aid -> do
      res <- query (retrieveAccount aid)
      case res of
        Right a -> writeBChan responseChan $ UAccount a
        Left err -> writeBChan responseChan $ UError (show err)
    FetchAccounts -> do
      res <- query (listAccounts_ Nothing)
      case res of
        Right as -> writeBChan responseChan $ UAccounts as
        Left err -> writeBChan responseChan $ UError (show err)
    FetchCategories -> do
      res <- query (getCategories <$> listCategories Nothing)
      case res of
        Right cs -> writeBChan responseChan $ UCategories cs
        Left err -> writeBChan responseChan $ UError (show err)
  where
    query = flip runClientM env

initialState ::
  ClientEnv ->
  BChan URequest ->
  State
initialState env requestChan =
  State
    { _transactions = empty,
      _accounts = L.list AccountList (Vec.fromList []) 1,
      _screen = ListZipper [helpScreen] mainScreen [],
      _categoryMap = empty,
      _clientEnv = env,
      _reqChan = requestChan,
      _version = appVersion
    }

appVersion :: Version
appVersion = Version "0.2"