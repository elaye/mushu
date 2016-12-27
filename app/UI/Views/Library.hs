{-# LANGUAGE OverloadedStrings #-}
module UI.Views.Library
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

import Brick.Main (continue)
import Brick.Types (BrickEvent(..))
import Brick.Widgets.List (list)
import Brick.Widgets.Core ((<=>), str)
import Brick.Widgets.Edit (renderEditor, handleEditorEvent)

-- import UI.Types (AppState, ViewType(..), playlist, activeView, config, VtyEvent(..), UIName(..))
import UI.Types (AppState, ViewType(..), artists, playlist, filterEditor, filterActive, activeView, UIName(..))
import qualified UI.Views.Main as Main
import qualified UI.Widgets.ArtistsList as ArtistsList
import Config (Config(..))

import Network.MPD (withMPD, Song(..), Id(..), playId)

type NextState = EventM UIName (Next AppState)

draw :: AppState -> [Widget UIName]
draw state = Main.draw state widget
  where 
    -- fzf = TextInput.mkWidget
    fzf = renderEditor True (state^.filterEditor)
    artistsWidget = ArtistsList.mkWidget (state^.artists)
    widget = case (state^.filterActive) of
      True -> fzf <=> artistsWidget
      False -> artistsWidget

event :: AppState -> BrickEvent UIName e -> EventM UIName (Next AppState)
event state (VtyEvent e) = case (state^.filterActive) of
  True -> case e of
    (V.EvKey V.KEsc []) -> continue (state & filterActive .~ False)
    ev -> do
      newFilterEditor <- handleEditorEvent ev (state^.filterEditor)
      continue (state & filterEditor .~ newFilterEditor)
  False -> case e of
    (V.EvKey (V.KChar '/') []) -> continue (state & filterActive .~ True)
    (V.EvKey (V.KChar 'j') []) -> next state
    (V.EvKey (V.KChar 'k') []) -> previous state
    -- (V.EvKey V.KEnter []) -> play state
    (V.EvKey (V.KChar 'q') []) -> halt state
    -- VtyEvent (V.EvKey (V.KChar '1') []) -> changeView state 1
    {-V.EvKey (V.KChar '-') [] -> delete-}
    ev -> listEvent ev state
event state _ = continue state

next :: AppState -> NextState
next state = continue $ state & artists %~ listMoveDown

previous :: AppState -> NextState
previous state = continue $ state & artists %~ listMoveUp

-- play :: AppState -> NextState
-- play state = case (state^.artists.listSelectedL) of
--   Nothing -> continue state
--   Just i -> do
--     let selectedArtist = (state^.artists.listElementsL) ! i
--     case (sgId selectedArtist) of
--       Nothing -> continue state
--       Just id -> do
--         _ <- liftIO $ withMPD $ playId id
--         continue state

listEvent :: V.Event -> AppState -> NextState
listEvent event state = continue =<< (\m -> state & artists .~ m) <$> handleListEvent event (state^.artists)

