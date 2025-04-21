{-# LANGUAGE OverloadedStrings #-}

module UI.MainView where

import Brick.Types
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core
import qualified Brick.Widgets.List as L
import Common
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import Lens.Micro.Platform
import Types
import Up.Model.Account
import Up.Model.Transaction

drawMain :: State -> ListZipper Focus -> [Widget Name]
drawMain st lz =
  [ C.center $
      vBox
        [ vLimit 6 $ drawAccounts st focus',
          drawTransactions st focus',
          vLimit 5 $ drawDetails st focus',
          drawBar st
        ]
  ]
  where
    focus' = lz ^. focus

drawBar :: State -> Widget Name
drawBar _st =
  vLimit 1 $
    hBox [mainButton, fill ' ', helpButton]
  where
    mainButton = str "1 Main  "
    helpButton = str "? Help"

drawDetails :: State -> Focus -> Widget Name
drawDetails st f =
  withBorderStyle unicode $
    borderWithLabel (str "Details") $
      case f of
        FocusAccounts -> drawAccountDetails st
        FocusTransactions -> drawTransactionDetails st
        _ -> fill ' '

drawAccountDetails :: State -> Widget Name
drawAccountDetails st = do
  maybe (fill ' ') (\(_, a) -> draw a) maybeAccount
  where
    maybeAccount = L.listSelectedElement (st ^. accounts)
    draw a =
      vBox
        [ hBox [name a, fill ' ', balance a, atype a],
          hBox [fill ' '],
          hBox [created a, fill ' ']
        ]
    _aid = str . T.unpack . accountId
    name = str . T.unpack . accountDisplayName
    atype = str . lpad 17 . T.pack . show . accountAccountType
    balance =
      str . lpad 17
        . showMoneyObject
        . accountBalance
    created =
      str . rpad' 12
        . fmap (T.append "Created at: " . T.pack)
        . fmtTime
        . T.unpack
        . accountCreatedAt

drawTransactionDetails :: State -> Widget Name
drawTransactionDetails st =
  maybe (fill ' ') (\(_, t) -> draw t) maybeTransaction
  where
    -- The id of the selected account
    aid = accountId . snd <$> L.listSelectedElement (st ^. accounts)
    -- The transactions associated with the selected account
    ts = aid >>= flip H.lookup (st ^. transactions)
    maybeTransaction = ts >>= L.listSelectedElement
    draw t =
      vBox
        [ hBox [desc t, fill ' ', created t, amount t],
          hBox [traw t, fill ' ', status t, famount t],
          hBox [roundup t, fill ' ', category t]
        ]
    _tid = str . T.unpack . transactionId
    desc = str . T.unpack . transactionDescription
    status = str . rpad 12 . T.pack . show . transactionStatus
    amount =
      str . lpad 17
        . showMoneyObject
        . transactionAmount
    famount =
      str . lpad' 17
        . fmap showMoneyObject
        . transactionForeignAmount
    _message = str . rpad' 40 . transactionMessage
    traw = str . rpad' 50 . transactionRawText

    created =
      str . rpad' 12
        . fmap T.pack
        . fmtTime
        . T.unpack
        . transactionCreatedAt

    roundup =
      str . rpad' 30
        . fmap (T.append "Round up: " . showMoneyObject . roundupAmount)
        . transactionRoundUp

    category =
      str . lpad' 15
        . foldMap catMap
        . transactionCategory
    catMap = flip H.lookup (st ^. categoryMap)

drawAccounts :: State -> Focus -> Widget Name
drawAccounts st f =
  withBorderStyle borderStyle $
    borderWithLabel (str "Accounts") $
      L.renderList drawAccountsElement True (st ^. accounts)
  where
    borderStyle = case f of
      FocusAccounts -> unicodeBold
      _ -> unicode

drawAccountsElement :: Bool -> Account -> Widget Name
drawAccountsElement _sel acc =
  let name = T.unpack $ accountDisplayName acc
      atype = lpad 17 (T.pack $ show $ accountAccountType acc)
      balance = lpad 17 $ showMoneyObject (accountBalance acc)
   in vLimit 1 $ hBox [str name, fill ' ', str balance, str atype]

drawTransactions :: State -> Focus -> Widget Name
drawTransactions st f =
  withBorderStyle borderStyle $
    borderWithLabel (str "Transactions") $
      maybe (fill ' ') (L.renderList drawTransactionElement True) ts
  where
    borderStyle = case f of
      FocusTransactions -> unicodeBold
      _ -> unicode
    -- The id of the selected account
    aid = accountId . snd <$> L.listSelectedElement (st ^. accounts)
    -- The transactions associated with the selected account
    ts = aid >>= flip H.lookup (st ^. transactions)

drawTransactionElement :: Bool -> Transaction -> Widget Name
drawTransactionElement _sel t =
  vLimit 1 $ hBox [desc t, fill ' ', created t, amount t]
  where
    desc = str . lpad 0 . transactionDescription
    amount = str . lpad 17 . showMoneyObject . transactionAmount
    created = str . lpad' 0 . fmap T.pack . fmtTime . T.unpack . transactionCreatedAt
