module App where

import Brick.Main
import Brick.Types ( BrickEvent(VtyEvent), EventM, Next )
import qualified Brick.Widgets.List as L
import qualified Brick.AttrMap as A
import Brick.Util (on)
import Control.Monad.IO.Class (liftIO)
import Graphics.Vty

import qualified Data.Vector as Vec
import Lens.Micro

import Types
import UI

import Up.API
import Up.Model.Account

import Servant.Client

-- App definition
app :: App State e Name
app = App
  { appDraw = drawUI
  , appChooseCursor = showFirstCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return
  , appAttrMap = const theMap
  }

-- Handling events
handleEvent :: State -> BrickEvent Name e -> EventM Name (Next State)
handleEvent s (VtyEvent e) =
  case s ^. isFocusing of
    True -> case e of
        EvKey KEsc [] -> continue s
        EvKey (KChar 'k') [] -> continue (s & focus %~ focusLeft & isFocusing .~ False)
        EvKey (KChar 'j') [] -> continue (s & focus %~ focusRight & isFocusing .~ False)
        EvKey KUp [] -> continue (s & focus %~ focusLeft & isFocusing .~ False)
        EvKey KDown [] -> continue (s & focus %~ focusRight & isFocusing .~ False)
        _ -> continue (s & isFocusing .~ False)
    False -> case e of
        EvKey (KChar 'q') [] -> halt s
        EvKey KEsc [] -> halt s
        EvKey (KChar 'w') [MCtrl] -> continue (s & isFocusing .~ True)
        EvKey KEnter [] -> case s ^. focus . currentZ of
          FocusAccounts -> do
            active <- case L.listSelectedElement (s ^. accounts) of
              Just (_, a) -> pure (accountId a)
              Nothing -> fail ""
            result <- liftIO $ query (s ^. clientEnv) (listTransactionsByAccount_ active Nothing Nothing Nothing Nothing)
            continue (s & transactions .~ (L.list TransactionList (Vec.fromList result) 0)
                        & focus %~ focusRight
                     )
          _ -> continue s
        ev -> case s ^. focus . currentZ of
          FocusTransactions -> do
            t' <- L.handleListEventVi L.handleListEvent ev (s ^. transactions)
            continue (s & transactions .~ t')
          FocusAccounts -> do
            a' <- L.handleListEventVi L.handleListEvent ev (s ^. accounts)
            continue (s & accounts .~ a')
          _ -> continue s
      where
handleEvent state _ = continue state

theMap :: A.AttrMap
theMap = A.attrMap defAttr
    [ (L.listSelectedAttr,    brightWhite `on` blue)
    ]

-- | Query the Up API
query :: ClientEnv -> ClientM a -> IO a
query e q = do
  r <- runClientM q e
  case r of
    Right a -> pure a
    Left err -> fail (show err)
