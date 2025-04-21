{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Types where

import Brick.BChan
import qualified Brick.Widgets.List as L
import Data.HashMap.Strict
import qualified Data.Text as T
import Lens.Micro.Platform
import Servant.Client (ClientEnv)
import Up.Model.Account
import Up.Model.Category
import Up.Model.Transaction

data Name
  = Name
  | TransactionList
  | AccountList
  | CategoryList
  deriving (Eq, Ord, Show)

-- | A Brick.Widgets.List.List of Accounts
type Accounts = L.List Name Account

-- | A Brick.Widgets.List.List of Transactions
type Transactions = L.List Name Transaction

type Categories' = L.List Name Category

data URequest
  = FetchTransaction AccountId
  | FetchAccount AccountId
  | FetchAccounts
  | FetchCategories

data UEvent
  = UConnect
  | UError String
  | UAccount Account
  | UAccounts [Account]
  | UTransactions (AccountId, [Transaction])
  | UCategories [Category]
  deriving (Eq, Show)

data Focus = FocusAccounts | FocusTransactions | FocusDetails
  deriving (Eq, Ord, Show)

-- * ListZipper

data ListZipper a = ListZipper
  { _leftCtx :: [a],
    _focus :: a,
    _rightCtx :: [a]
  }
  deriving (Show, Ord, Eq)

makeLenses ''ListZipper

focusLeft :: ListZipper a -> ListZipper a
focusLeft (ListZipper (l : ls) x rs) = ListZipper ls l (x : rs)
focusLeft (ListZipper []       x rs) = ListZipper ls l [x]
  where
    (l : ls) = reverse rs

focusRight :: ListZipper a -> ListZipper a
focusRight (ListZipper ls x (r : rs)) = ListZipper (x : ls) r rs
focusRight (ListZipper ls x []      ) = ListZipper [x]      r rs
  where
    (r : rs) = reverse ls

-- | focusRight until the focus satisfies the predicate.
focusFind :: (a -> Bool) -> ListZipper a -> ListZipper a
focusFind p lz
  | p (lz ^. focus) = lz
  | otherwise = focusFind p $ focusRight lz

newtype Version = Version {_versionNumber :: String}
  deriving (Eq, Ord, Show)

makeLenses ''Version

data Mode = NormalMode | ViewportMode
  deriving (Eq, Ord, Show)

data Display
  = MainView (ListZipper Focus) Mode
  | HelpView
  deriving (Eq, Ord, Show)

data Tag
  = MainTag
  | HelpTag
  deriving (Eq, Ord, Show)

-- A Screen is a tag and a view
data Screen = Screen
  { _tag :: Tag,
    _display :: Display
  }

makeLenses ''Screen

mainScreen :: Screen
mainScreen = Screen MainTag (MainView (ListZipper [] FocusAccounts [FocusTransactions]) NormalMode)

helpScreen :: Screen
helpScreen = Screen HelpTag HelpView

data State = State
  { _accounts :: Accounts,
    _transactions :: HashMap AccountId Transactions,
    _categoryMap :: HashMap CategoryId T.Text,
    _screen :: ListZipper Screen,
    _clientEnv :: ClientEnv,
    _reqChan :: BChan URequest,
    _version :: Version
  }

makeLenses ''State

setScreen :: Tag -> State -> State
setScreen t st = st & screen %~ focusFind ((t ==) . (^. tag))

setMainScreen :: State -> State
setMainScreen = setScreen MainTag

setHelpScreen :: State -> State
setHelpScreen = setScreen HelpTag

setDisplay :: Display -> State -> State
setDisplay v st = st & (screen . focus . display) .~ v