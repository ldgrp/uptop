module App where

import Brick.Main
import qualified Brick.Widgets.List as L
import qualified Brick.AttrMap as A
import Brick.Util (on)
import Graphics.Vty

import Event
import Types
import UI

-- App definition
app :: App State UEvent Name
app = App
  { appDraw = drawUI
  , appChooseCursor = showFirstCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return
  , appAttrMap = const theMap
  }


theMap :: A.AttrMap
theMap = A.attrMap defAttr
    [ (L.listSelectedAttr,    brightWhite `on` blue)
    ]