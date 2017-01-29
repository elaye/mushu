module UI.Views.Playlist
( draw
, event
) where

import ClassyPrelude hiding ((<>))
import Data.Monoid ((<>))

import Brick.Types (Widget(..), EventM, Next)
import qualified Graphics.Vty as V
import Brick.Main (continue, halt)
import Brick.Widgets.List
  ( listSelectedL
  , listRemove
  , listMoveUp
  , listMoveDown
  , listElementsL
  , handleListEvent
  , List
  )

import Data.Vector ((!))
import Data.List ((!!))
import Data.HashMap.Strict (elems)
import Lens.Micro.Platform ((^.), (%~), (&), (.~))

import Brick.Types (BrickEvent(..))
import Brick.Widgets.List (list)
import Brick.Widgets.Core ((<+>))

import UI.Types (AppState, ViewType(..), playlist, activeView, UIName(..))
import qualified UI.Views.Main as Main
import qualified UI.Widgets.Playlist as Playlist

import Network.MPD (withMPD, Song(..), Id(..), playId)

type NextState = EventM UIName (Next AppState)

draw :: AppState -> [Widget UIName]
draw state = Main.draw state playlst
  where playlst = Playlist.mkWidget (state^.playlist)

-- event :: AppState -> VtyEvent -> EventM UIName (Next AppState)
event :: AppState -> BrickEvent UIName e -> EventM UIName (Next AppState)
event state (VtyEvent e) = case e of
    (V.EvKey (V.KChar 'j') []) -> next state
    (V.EvKey (V.KChar 'k') []) -> previous state
    (V.EvKey V.KEnter []) -> play state
    (V.EvKey (V.KChar 'q') []) -> halt state
    -- VtyEvent (V.EvKey (V.KChar '1') []) -> changeView state 1
    {-V.EvKey (V.KChar '-') [] -> delete-}
    ev -> listEvent ev state
event state _ = continue state

next :: AppState -> NextState
next state = continue $ state & playlist %~ listMoveDown

previous :: AppState -> NextState
previous state = continue $ state & playlist %~ listMoveUp

-- openAndMarkAsRead :: AppState -> NextState
-- openAndMarkAsRead state = case (state^.mails.listSelectedL) of
--   Nothing -> continue state
--   Just i -> do
--     let selectedMail = (state^.mails.listElementsL) ! i
--     _ <- liftIO $ void $ markAsRead selectedMail
--     continue $ open state selectedMail

play :: AppState -> NextState
play state = case (state^.playlist.listSelectedL) of
  Nothing -> continue state
  Just i -> do
    let selectedSong = (state^.playlist.listElementsL) ! i
    case (sgId selectedSong) of
      Nothing -> continue state
      Just id -> do
        _ <- liftIO $ withMPD $ playId id
        continue state

-- open :: AppState -> Mail -> AppState
-- open state selectedMail = state
--   & mail .~ Just selectedMail
--   & activeView .~ MailView

listEvent :: V.Event -> AppState -> NextState
listEvent event state = continue =<< (\m -> state & playlist .~ m) <$> handleListEvent event (state^.playlist)

