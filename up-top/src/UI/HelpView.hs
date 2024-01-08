{-# LANGUAGE OverloadedStrings #-}

module UI.HelpView where

import Brick.Types
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Core
import Lens.Micro.Platform
import Types

drawHelp :: State -> [Widget Name]
drawHelp st =
  [ center $
      padLeftRight 2 $
        hLimit 50 $
          vBox
            [ str $ "uptop " <> st ^. version . versionNumber <> " - (C) 2020 Leo Orpilla III",
              vLimit 1 $ fill ' ',
              drawControls,
              vLimit 1 $ fill ' ',
              drawVimControls,
              vLimit 1 $ fill ' ',
              str "Press any key to return."
            ]
  ]

drawControls :: Widget Name
drawControls =
  vBox
    [ hBorderWithLabel (str "Controls"),
      vLimit 1 $ fill ' ',
      vLimit 1 $ str "← ↑ ↓ →" <+> fill ' ' <+> str "Left/Up/Down/Right",
      vLimit 1 $ str "PageUp PageDown" <+> fill ' ' <+> str "Page Up/Page Down",
      vLimit 1 $ str "Home End" <+> fill ' ' <+> str "Go to first/Go to last",
      vLimit 1 $ str "C-w ↑" <+> fill ' ' <+> str "Viewport Up",
      vLimit 1 $ str "C-w ↓" <+> fill ' ' <+> str "Viewport Down",
      vLimit 1 $ str "Esc" <+> fill ' ' <+> str "Exit"
    ]

drawVimControls :: Widget Name
drawVimControls =
  vBox
    [ hBorderWithLabel (str "Vim-style"),
      vLimit 1 $ fill ' ',
      vLimit 1 $ str "h j k l" <+> fill ' ' <+> str "Left/Up/Down/Right",
      vLimit 1 $ str "C-b C-f" <+> fill ' ' <+> str "Page Up/Page Down",
      vLimit 1 $ str "g G" <+> fill ' ' <+> str "Go to first/Go to last",
      vLimit 1 $ str "C-w k" <+> fill ' ' <+> str "Viewport Up",
      vLimit 1 $ str "C-w j" <+> fill ' ' <+> str "Viewport Down",
      vLimit 1 $ str "q" <+> fill ' ' <+> str "Exit"
    ]
