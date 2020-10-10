{-# LANGUAGE TemplateHaskell #-}

module Types where 

import Brick.BChan
import qualified Brick.Widgets.List as L
import Data.HashMap.Strict
import qualified Data.Text as T
import Lens.Micro.TH
import Up.Model.Account
import Up.Model.Transaction
import Up.Model.Category
import Servant.Client ( ClientEnv )


data Name = Name | TransactionList | AccountList | CategoryList
  deriving (Eq, Ord, Show)

-- | A Brick.Widgets.List.List of Accounts
type Accounts = L.List Name Account

-- | A Brick.Widgets.List.List of Transactions
type Transactions = L.List Name Transaction

type Categories' = L.List Name Category

data Focus = FocusAccounts | FocusTransactions | FocusDetails
data Screen = AuthScreen | MainScreen

data ListZipper a = ListZipper
  { _leftZ :: [a]
  , _rightZ :: [a]
  , _currentZ :: a
  } deriving (Show, Eq)

makeLenses ''ListZipper

focusLeft :: ListZipper a -> ListZipper a
focusLeft (ListZipper (l:ls) rs x) = ListZipper ls (x:rs) l
focusLeft (ListZipper [] rs x) = ListZipper ls [x] l 
  where (l:ls) = reverse rs 

focusRight :: ListZipper a -> ListZipper a
focusRight (ListZipper ls (r:rs) x) = ListZipper (x:ls) rs r
focusRight (ListZipper ls [] x) = ListZipper [x] rs r 
  where (r:rs) = reverse ls 

data State = State 
  { _accounts :: Accounts
  , _transactions :: Transactions
  , _categoryMap :: HashMap CategoryId T.Text
  , _focus :: ListZipper Focus
  , _screen :: ListZipper Screen
  , _clientEnv :: ClientEnv
  , _isFocusing :: Bool
  } 

makeLenses ''State
