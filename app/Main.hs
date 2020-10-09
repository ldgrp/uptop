{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad (void, liftM2)

import Brick.Main
import qualified Brick.Widgets.List as L
import qualified Data.Vector as Vec
import Data.HashMap.Strict
import Graphics.Vty

import Up

import Up.API
import Up.Model.Account
import Up.Model.Transaction
import Up.Model.Token
import Up.Model.Category

import App
import Auth
import Types

import qualified Data.Text as T

import Servant.Client
import System.Exit
import System.Environment

main :: IO ()
main = do
  let buildVty = mkVty defaultConfig
  initialVty <- buildVty
  
  envToken <- lookupEnv "UP_BANK_TOKEN"

  (authEvent, vty) <- interactiveAuth initialVty buildVty envToken

  token <- case authEvent of
    Just (Success (AuthInfo tok)) -> pure $ T.unpack tok
    Just _ae -> do
      shutdown vty
      exitSuccess
    Nothing -> do
      shutdown vty
      exitSuccess

  env <- mkUpClient (Token token)

  catList <- query env (getCategories <$> listCategories Nothing)
  let catMap = fromList (fmap (liftM2 (,) categoryId categoryName) catList)
  acc <- query env (listAccounts_ Nothing)

  void $ customMainWithVty vty buildVty (Nothing) app (initialState [] acc catMap env)
  shutdown vty

initialState :: [Transaction] 
             -> [Account]
             -> HashMap CategoryId T.Text
             -> ClientEnv
             -> State
initialState t a catMap env = State 
  { _transactions = L.list TransactionList (Vec.fromList t) 1
  , _accounts = L.list AccountList (Vec.fromList a) 1
  , _focus = ListZipper [] [FocusTransactions] FocusAccounts
  , _screen = ListZipper [] [MainScreen] AuthScreen
  , _isFocusing = False
  , _categoryMap = catMap
  , _clientEnv = env
  }