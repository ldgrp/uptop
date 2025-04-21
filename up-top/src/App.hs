module App where

import qualified Brick.AttrMap as A
import Brick.Main
import Brick.Util (on)
import qualified Brick.Widgets.List as L
import Event
import Graphics.Vty
import Types
import UI

-- App definition
app :: App State UEvent Name
app =
  App
    { appDraw = drawUI,
      appChooseCursor = showFirstCursor,
      appHandleEvent = handleEvent,
      appStartEvent = pure (),
      appAttrMap = const theMap
    }

theMap :: A.AttrMap
theMap =
  A.attrMap
    defAttr
    [ (L.listSelectedAttr, brightWhite `on` blue)
    ]