module UI.Views.Help
( draw
) where

import Brick.Types (Widget)
import Brick.Widgets.Core (str)

draw :: [Widget n]
draw = [ str "help"
       , str "j    move down"
       , str "k    move up"
       , str "q    quit app or close current view"
       , str "?    toggle help"
       ]
