module UI where

import Brick.Types
import Lens.Micro.Platform

import Types
import UI.HelpView
import UI.MainView

drawUI :: State -> [Widget Name]
drawUI st =
  case st ^. (screen . focus . display) of
    MainView lz _m -> drawMain st lz
    HelpView -> drawHelp st
