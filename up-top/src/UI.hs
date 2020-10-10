{-# LANGUAGE OverloadedStrings #-}

module UI where

import Brick.Types
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core
    ( fill, hBox, str, vBox, vLimit, withBorderStyle )

import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import Data.Time.LocalTime
import Data.Time.Format

import Lens.Micro
import Types

import Up.Model.Account
import Up.Model.MoneyObject
import Up.Model.Transaction



-- Drawing
drawUI :: State -> [Widget Name]
drawUI state = 
  [ C.center $ vBox [ 
        vLimit 6 $ drawAccounts state
      , drawTransactions state
      , vLimit 5 $ drawDetails state
      , str $ "F1 Help " <> "ESC Quit"
    ]
  ]

drawDetails :: State -> Widget Name
drawDetails state = 
  withBorderStyle borderStyle $
  borderWithLabel (str "Details") $
  case state ^. focus ^. currentZ of
    FocusAccounts -> drawAccountDetails state
    FocusTransactions -> drawTransactionDetails state
    _ -> fill ' '
  where 
    borderStyle = case state ^. focus . currentZ of
      FocusDetails -> unicodeBold
      _ -> unicode

drawAccountDetails :: State -> Widget Name
drawAccountDetails state = do
  maybe (fill ' ') (\(_, a) -> draw a) maybeAccount
  where 
    maybeAccount = L.listSelectedElement (state ^. accounts)
    draw a = vBox [hBox [name a, fill ' ', balance a, atype a]
                  ,hBox [fill ' ']
                  ,hBox [created a, fill ' ']
                  ]
    aid = str . T.unpack . accountId
    name = str . rpad 25 . accountDisplayName
    atype = str . lpad 17 . T.pack . show . accountAccountType
    balance = str . lpad 17 
                  . showMoneyObject 
                  . accountBalance
    created = str . rpad' 12 
                  . fmap (T.append "Created at: ")
                  . fmap T.pack 
                  . fmtTime 
                  . T.unpack 
                  . accountCreatedAt

drawTransactionDetails :: State -> Widget Name
drawTransactionDetails state =
  maybe (fill ' ') (\(_, t) -> draw t) maybeTransaction
  where 
    maybeTransaction = L.listSelectedElement (state ^. transactions)
    draw t = vBox [hBox [desc t, fill ' ', created t, amount t]
                  ,hBox [raw t, fill ' ', status t, famount t]
                  ,hBox [roundup t, fill ' ', category t]
                  ]
    tid = str . T.unpack . transactionId
    desc = str . rpad 30 . transactionDescription
    status = str . rpad 12 . T.pack . show . transactionStatus
    amount = str . lpad 17 
                 . showMoneyObject 
                 . transactionAmount
    famount = str . lpad' 17 
                  . fmap showMoneyObject 
                  . transactionForeignAmount
    message = str . rpad' 40 . transactionMessage
    raw = str . rpad' 50 . transactionRawText

    created = str . rpad' 12 
                  . fmap T.pack 
                  . fmtTime 
                  . T.unpack 
                  . transactionCreatedAt

    roundup = str . rpad' 30 
                  . fmap (T.append "Round up: ") 
                  . fmap showMoneyObject 
                  . fmap roundupAmount 
                  . transactionRoundUp

    category = str . lpad' 15 
                   . foldMap catMap 
                   . transactionCategory
    catMap = flip H.lookup (state ^. categoryMap)

drawAccounts :: State -> Widget Name
drawAccounts state = 
  withBorderStyle borderStyle $
  borderWithLabel (str "Accounts") $
  L.renderList drawAccountsElement True (state ^. accounts)
  where 
    borderStyle = case state ^. focus . currentZ of
      FocusAccounts -> unicodeBold
      _ -> unicode

drawAccountsElement :: Bool -> Account -> Widget Name
drawAccountsElement _sel acc = 
  let name = rpad 19 (accountDisplayName acc)
      atype = lpad 17 (T.pack $ show $ accountAccountType acc)
      balance = lpad 17 $ showMoneyObject (accountBalance acc)
  in vLimit 1 $ hBox [ str name, fill ' ', str balance, str atype ]

drawTransactions :: State -> Widget Name
drawTransactions state = 
  withBorderStyle borderStyle $
  borderWithLabel (str "Transactions") $
  L.renderList drawTransactionElement True (state ^. transactions)
  where 
    borderStyle = case state ^. focus . currentZ of
      FocusTransactions -> unicodeBold
      _ -> unicode

drawTransactionElement :: Bool -> Transaction -> Widget Name
drawTransactionElement _sel t = 
  vLimit 1 $ hBox [desc t, fill ' ', created t, amount t]
  where desc = str . lpad 0 . transactionDescription
        amount = str. lpad 17 . showMoneyObject . transactionAmount
        created = str . lpad' 0 . fmap T.pack . fmtTime . T.unpack . transactionCreatedAt

showMoneyObject :: MoneyObject -> T.Text
showMoneyObject mo = T.intercalate " " [value mo , moCurrencyCode mo]
  where value x 
          | T.isPrefixOf "-" (moValue x) = moValue x 
          | otherwise = T.cons '+' (moValue x)

fmtTime :: String -> Maybe String
fmtTime = fmap (formatTime defaultTimeLocale "%d %b %R") . utcTime
  where utcTime :: String -> Maybe LocalTime
        utcTime = parseTimeM True defaultTimeLocale "%FT%T%Ez"

lpad :: Int -> T.Text -> String
lpad i t = T.unpack $ T.justifyRight i ' ' t

rpad :: Int -> T.Text -> String
rpad i t = T.unpack $ T.justifyLeft i ' ' t

lpad' :: Int -> Maybe T.Text -> String
lpad' i t = lpad i t'
  where t' = maybe "" id t :: T.Text

rpad' :: Int -> Maybe T.Text -> String
rpad' i t = rpad i t'
  where t' = maybe "" id t :: T.Text
