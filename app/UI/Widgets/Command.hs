module UI.Widgets.Command
( mkWidget
) where

import ClassyPrelude

import Brick.Types (Widget)
import Brick.Widgets.Core (str)
import Brick.Widgets.Center (hCenter)

import UI.Types (UIName)

mkWidget :: Widget UIName
mkWidget = hCenter $ str "Command"
