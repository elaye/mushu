module UI.Views.Main
( draw
) where

import ClassyPrelude

import Brick.Widgets.Center (vCenter, hCenter)

import Brick.Types (Widget)
import Brick.Widgets.Core (vBox)

import Lens.Micro.Platform ((^.))

import qualified UI.Widgets.Status as Status
import qualified UI.Widgets.Help as Help
import qualified UI.Widgets.Command as Command

import UI.Types (AppState, UIName)

draw :: AppState -> Widget UIName -> [Widget UIName]
draw state widget = [ui]
    where
        {-total = str $ show $ Vec.length $ mails^.(L.listElementsL)-}
        ui = vCenter $ vBox widgets
        widgets = [ Help.mkWidget
                  , hCenter widget
                  , Status.mkWidget state
                  , Command.mkWidget
                  ]
