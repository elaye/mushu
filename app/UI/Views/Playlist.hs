module UI.Views.Playlist
( draw
, event
) where

import ClassyPrelude hiding ((<>))

import Brick.Types (BrickEvent(..), Widget(..), EventM, Next)
import qualified Graphics.Vty as V
import Brick.Main (continue, halt)
import Lens.Micro.Platform ((^.), (&), (.~))

import UI.Types (AppState, playlistStateL)
import qualified UI.Views.Main as Main
import qualified UI.Widgets.Playlist as Playlist

type NextState n = EventM n (Next (AppState n))

draw :: (Show n, Ord n) => AppState n -> [Widget n]
draw state = Main.draw state playlst
  where playlst = Playlist.mkWidget (state^.playlistStateL)

event :: (Ord n) => AppState n -> BrickEvent n e -> NextState n
event state (VtyEvent e) = case e of
    (V.EvKey (V.KChar 'q') []) -> halt state
    ev -> do
      newPlaylistState <- Playlist.handleEvent ev (state^.playlistStateL)
      continue $ state & playlistStateL .~ newPlaylistState
event state _ = continue state
