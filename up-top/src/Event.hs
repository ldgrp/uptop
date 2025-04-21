{-# LANGUAGE LambdaCase #-}
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
import Lens.Micro.Platform
import Types
import Up.Model.Account
import Up.Model.Category

-- Handling events
handleEvent :: BrickEvent Name UEvent -> EventM Name State ()
handleEvent (VtyEvent e) = do
  use (screen . focus . display) >>= \case
    MainView lz ViewportMode ->
      case e of
        EvKey KEsc []        -> pure ()
        EvKey (KChar 'k') [] -> modify (setDisplay (MainView (focusLeft lz) NormalMode))
        EvKey (KChar 'j') [] -> modify (setDisplay (MainView (focusRight lz) NormalMode))
        EvKey KUp []         -> modify (setDisplay (MainView (focusLeft lz) NormalMode))
        EvKey KDown []       -> modify (setDisplay (MainView (focusRight lz) NormalMode))
        _                    -> modify (setDisplay (MainView lz NormalMode))
    MainView lz NormalMode ->
      case e of
        EvKey KEsc []             -> halt
        EvKey (KChar 'q') []      -> halt
        EvKey (KChar '?') []      -> modify setHelpScreen
        EvKey (KChar 'w') [MCtrl] -> modify (setDisplay (MainView lz ViewportMode))
        EvKey KEnter []           -> case lz ^. focus of
          FocusAccounts -> do
            aid <- use (accounts . to L.listSelectedElement) >>= \case
              Just (_, a) -> pure (accountId a)
              Nothing     -> error "" -- TODO: No more monad fail
            ch <- use reqChan
            liftIO $ writeBChan ch $ FetchTransaction aid
            modify (setDisplay (MainView (focusRight lz) NormalMode))
          _ -> pure ()
        ev -> case lz ^. focus of
          FocusTransactions -> do
            -- The id of the selected account, if it exists
            preuse (accounts . L.listSelectedElementL . to accountId) >>= mapM_ (\aid ->
              zoom (transactions . ix aid) $
                L.handleListEventVi L.handleListEvent ev)
          FocusAccounts     -> zoom accounts $ L.handleListEventVi L.handleListEvent ev
          _                 -> pure ()
    HelpView ->
      modify setMainScreen

handleEvent (AppEvent (UAccounts as)) = do
  accounts     .= L.list AccountList (Vec.fromList as) 1
  transactions %= H.union transactions'
  where
    transactions' = H.fromList [(accountId a, L.list TransactionList (Vec.fromList []) 1) | a <- as]

handleEvent (AppEvent (UTransactions (aid, ts))) =
  transactions %= H.insert aid (L.list TransactionList (Vec.fromList ts) 1)

handleEvent (AppEvent (UCategories cs)) =
  categoryMap .= H.fromList (fmap (liftM2 (,) categoryId categoryName) cs)

handleEvent (AppEvent _) = pure ()
handleEvent _ = pure ()
