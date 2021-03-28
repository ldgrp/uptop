{-# LANGUAGE OverloadedStrings #-}

module UI where

import Brick.Types
import Lens.Micro

import Types
import UI.HelpView
import UI.MainView

drawUI :: State -> [Widget Name]
drawUI st = 
  case st ^. (screen . focus . view) of
    MainView lz _m -> drawMain st lz
    HelpView -> drawHelp st
