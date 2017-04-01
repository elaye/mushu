module UI.Views.Help
( draw
) where

import ClassyPrelude
import Brick.Types (Widget)
import Brick.Widgets.Core (str, vBox)

draw :: [Widget n]
draw = [vBox $ str <$> [ " Help"
       , " "
       , " General"
       , " "
       , "   j      move down"
       , "   k      move up"
       , "   h      move left"
       , "   l      move right"
       , " "
       , "   c      clear playlist"
       , "   p      play/pause"
       , "   -      decrease volume"
       , "   +      increase volume"
       , " "
       , "   1      show playlist"
       , "   2      show library"
       , " "
       , "   q      quit app or close current view"
       , "   ?      toggle help"
       , " "
       , " Library"
       , " "
       , "   a        add to playlist"
       , "   <Enter>  add to playlist and play"
       , "   t        toggle between artist/albums and albums view"
       , "   /        filter"
       , "   <Esc>    disable filter if active"
       ]]
