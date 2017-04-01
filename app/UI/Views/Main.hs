module UI.Views.Main
( draw
) where

import ClassyPrelude

import Brick.Widgets.Center (vCenter, hCenter)
import Brick.Widgets.Border (hBorder)

import Brick.Types (Widget, Padding(..))
import Brick.Widgets.Core (vBox, hLimit, str, padLeft, padRight, (<=>), (<+>))

import Lens.Micro.Platform ((^.))

import qualified UI.Widgets.Status as Status
import qualified UI.Widgets.Help as Help
import qualified UI.Widgets.Command as Command
import qualified UI.Widgets.Notification as Notification
import UI.Widgets.Playlist (playingSongL)

import UI.Types

draw :: AppState n -> Widget n -> [Widget n]
draw state widget = [ui]
    where
        {-total = str $ show $ Vec.length $ mails^.(L.listElementsL)-}
        ui = vCenter $ vBox widgets
        -- Cheap centering
        view = hBorder <=> (str "       " <+> hCenter activeViewWidget <+> str "?: Help")
        activeViewWidget = str $ show (state^.activeView)
        widgets = [--Help.mkWidget
                   Status.mkWidget state (state^.playlistStateL.playingSongL)
                  , hBorder
                  , hCenter widget
                  , view
                  , Notification.mkWidget (state^.notificationState)
                  -- , Command.mkWidget
                  ]

