module Event where

import Brick.Main
import Brick.Types ( BrickEvent(VtyEvent), EventM, Next )
import qualified Brick.Widgets.List as L
import Control.Monad.IO.Class (liftIO)
import Graphics.Vty

import qualified Data.Vector as Vec
import Lens.Micro
import Servant.Client

import Types

import Up.API
import Up.Model.Account


-- Handling events
handleEvent :: State -> BrickEvent Name e -> EventM Name (Next State)
handleEvent s (VtyEvent e) =
  case s ^. screen ^. focus ^. view of

    MainView lz ViewportMode ->
      case e of 
        EvKey KEsc [] -> continue s
        EvKey (KChar 'k') [] -> continue (setView (MainView (focusLeft lz) NormalMode) s)
        EvKey (KChar 'j') [] -> continue (setView (MainView (focusRight lz) NormalMode) s)
        EvKey KUp [] -> continue (setView (MainView (focusLeft lz) NormalMode) s)
        EvKey KDown [] -> continue (setView (MainView (focusRight lz) NormalMode) s)
        _ -> continue (setView (MainView lz NormalMode) s)

    MainView lz NormalMode -> 
      case e of
        EvKey KEsc        [] -> halt s
        EvKey (KChar 'q') [] -> halt s
        EvKey (KChar '?') [] -> continue (setHelpScreen s)
        EvKey (KChar 'w') [MCtrl] -> continue (setView (MainView lz ViewportMode) s)

        EvKey KEnter [] -> case lz ^. focus of
          FocusAccounts -> do
            active <- case L.listSelectedElement (s ^. accounts) of
              Just (_, a) -> pure (accountId a)
              Nothing -> fail ""
            result <- liftIO $ query (s ^. clientEnv) (listTransactionsByAccount_ active Nothing Nothing Nothing Nothing)
            continue (s & transactions .~ (L.list TransactionList (Vec.fromList result) 0))
          _ -> continue s

        ev -> case lz ^. focus of
          FocusTransactions -> do
            t' <- L.handleListEventVi L.handleListEvent ev (s ^. transactions)
            continue (s & transactions .~ t')
          FocusAccounts -> do
            a' <- L.handleListEventVi L.handleListEvent ev (s ^. accounts)
            continue (s & accounts .~ a')
          _ -> continue s

    HelpView -> 
      case e of
        EvKey KEsc        [] -> continue (setMainScreen s)
        EvKey (KChar 'q') [] -> continue (setMainScreen s)
        EvKey (KChar '1') [] -> continue (setMainScreen s)
        _ -> continue s

handleEvent state _ = continue state

-- | Query the Up API
query :: ClientEnv -> ClientM a -> IO a
query e q = do
  r <- runClientM q e
  case r of
    Right a -> pure a
    Left err -> fail (show err)