module Event where

import Brick.BChan
import Brick.Main
import Brick.Types
import qualified Brick.Widgets.List as L
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as Vec
import Graphics.Vty
import Lens.Micro
import Lens.Micro.Mtl ((.=), (%=), use)
import Types
import Up.Model.Account
import Up.Model.Category

-- Handling events
handleEvent :: BrickEvent Name UEvent -> EventM Name State ()
handleEvent (VtyEvent e) = do
  currentView <- use (screen . focus . view)
  case currentView of
    MainView lz ViewportMode ->
      case e of
        EvKey KEsc [] -> pure ()
        EvKey (KChar 'k') [] -> setView $ MainView (focusLeft lz) NormalMode
        EvKey (KChar 'j') [] -> setView $ MainView (focusRight lz) NormalMode
        EvKey KUp [] -> setView $ MainView (focusLeft lz) NormalMode
        EvKey KDown [] -> setView $ MainView (focusRight lz) NormalMode
        _ -> setView $ MainView lz NormalMode
    MainView lz NormalMode ->
      case e of
        EvKey KEsc [] -> halt
        EvKey (KChar 'q') [] -> halt
        EvKey (KChar '?') [] -> switchToHelpScreen
        EvKey (KChar 'w') [MCtrl] -> setView $ MainView lz ViewportMode
        EvKey KEnter [] -> case lz ^. focus of
          FocusAccounts -> do
            accountsList <- use accounts
            case L.listSelectedElement accountsList of
              Just (_, a) -> do
                let aid = accountId a
                chan <- use reqChan
                liftIO $ writeBChan chan $ FetchTransaction aid
                setView $ MainView (focusRight lz) NormalMode
              Nothing -> pure ()
          _ -> pure ()
        ev -> case lz ^. focus of
          FocusTransactions -> do
            accountsList <- use accounts
            transactionsList <- use transactions
            case accountId . snd <$> L.listSelectedElement accountsList of
              Just aid ->
                case H.lookup aid transactionsList of
                  Just tList -> do
                    (tList', _) <- nestEventM tList $ 
                      L.handleListEventVi L.handleListEvent ev
                    transactions %= H.insert aid tList'
                  Nothing -> pure ()
              Nothing -> pure ()
          FocusAccounts -> do
            accountsList <- use accounts
            (newAccounts, _) <- nestEventM accountsList $ 
              L.handleListEventVi L.handleListEvent ev
            accounts .= newAccounts
          _ -> pure ()
    HelpView ->
      switchToMainScreen
handleEvent (AppEvent (UAccounts as)) = do
  accounts .= L.list AccountList (Vec.fromList as) 1
  transactions %= H.union transactions'
  where
    transactions' = H.fromList [(accountId a, L.list TransactionList (Vec.fromList []) 1) | a <- as]
handleEvent (AppEvent (UTransactions (aid, ts))) =
  transactions %= H.insert aid (L.list TransactionList (Vec.fromList ts) 1)
handleEvent (AppEvent (UCategories cs)) =
  categoryMap .= H.fromList (fmap (liftM2 (,) categoryId categoryName) cs)
handleEvent (AppEvent _) = pure ()
handleEvent _ = pure ()