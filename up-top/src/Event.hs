module Event where

import Brick.BChan 
import Brick.Main
import Brick.Types 
import qualified Brick.Widgets.List as L
import Control.Monad.IO.Class (liftIO)
import Control.Monad
import qualified Data.Vector as Vec
import qualified Data.HashMap.Strict as H
import Graphics.Vty
import Lens.Micro

import Types

import Up.Model.Account
import Up.Model.Category

-- Handling events
handleEvent :: State -> BrickEvent Name UEvent -> EventM Name (Next State)
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
            aid <- case L.listSelectedElement (s ^. accounts) of
              Just (_, a) -> pure (accountId a)
              Nothing -> fail ""
            liftIO $ writeBChan (s ^. reqChan) $ FetchTransaction aid
            continue (setView (MainView (focusRight lz) NormalMode) s)
          _ -> continue s
        ev -> case lz ^. focus of
          FocusTransactions -> do
            -- The id of the selected account
            case accountId <$> snd <$> L.listSelectedElement (s ^. accounts) of
              Just aid -> 
                case H.lookup aid (s ^. transactions) of
                  Just tList -> do
                    tList' <- L.handleListEventVi L.handleListEvent ev tList
                    continue (s & transactions %~ H.insert aid tList')
                  Nothing -> continue s
              Nothing -> continue s
          FocusAccounts -> do
            a' <- L.handleListEventVi L.handleListEvent ev (s ^. accounts)
            continue (s & accounts .~ a')
          _ -> continue s

    HelpView -> 
      case e of
        _ -> continue (setMainScreen s)
handleEvent s (AppEvent (UAccounts as)) =
  continue (s & accounts .~ L.list AccountList (Vec.fromList as) 1
              & transactions %~ H.union transactions')
  where transactions' = H.fromList [(accountId a, L.list TransactionList (Vec.fromList []) 1) | a <- as]
handleEvent s (AppEvent (UTransactions (aid, ts))) = 
  continue (s & transactions %~ H.insert aid (L.list TransactionList (Vec.fromList ts) 1))
handleEvent s (AppEvent (UCategories cs)) = 
  continue (s & categoryMap .~ H.fromList (fmap (liftM2 (,) categoryId categoryName) cs))
handleEvent s (AppEvent _) = continue s
handleEvent s _ = continue s