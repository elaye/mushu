module UI.Views.Main
( draw
) where

import ClassyPrelude

import Brick.Widgets.Center (vCenter, hCenter)
import Brick.Widgets.Border (hBorder)

import Brick.Types (Widget)
import Brick.Widgets.Core (vBox, str, (<=>))

import Lens.Micro.Platform ((^.))

import qualified UI.Widgets.Status as Status
import qualified UI.Widgets.Help as Help
import qualified UI.Widgets.Command as Command

import UI.Types

draw :: AppState n -> Widget n -> [Widget n]
draw state widget = [ui]
    where
        {-total = str $ show $ Vec.length $ mails^.(L.listElementsL)-}
        ui = vCenter $ vBox widgets
        view = hBorder <=> (hCenter (str $ show (state^.activeView)))
        widgets = [--Help.mkWidget
                   Status.mkWidget state
                  , hBorder
                  , hCenter widget
                  , view
                  -- , Command.mkWidget
                  ]

