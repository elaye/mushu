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
import Brick.Widgets.Edit (Editor, applyEdit, renderEditor, handleEditorEvent, getEditContents)

import Data.Text.Zipper (textZipper)

-- import UI.Types (AppState, ViewType(..), playlist, activeView, config, VtyEvent(..), UIName(..))
import UI.Types (AppState, ViewType(..), artists, playlist, filterEditor, filterActive, filterFocused, filteredArtists, activeView, UIName(..))
import qualified UI.Views.Main as Main
import qualified UI.Widgets.ArtistsList as ArtistsList
import Config (Config(..))

import Network.MPD (withMPD, Song(..), Id(..), playId, Artist, toString)
import Text.Fuzzy (simpleFilter)

type NextState = EventM UIName (Next AppState)

draw :: AppState -> [Widget UIName]
draw state = Main.draw state widget
  where 
    -- fzf = TextInput.mkWidget
    fzf = renderEditor True (state^.filterEditor)
    artistsWidget = ArtistsList.mkWidget $ state^.filteredArtists
      -- case (state^.filterActive) of
      --   -- True -> getFilteredArtists state
      --   True -> state^.filteredArtists
      --   False -> state^.artists
    widget = case (state^.filterActive) of
      True -> fzf <=> artistsWidget
      False -> artistsWidget

getFilteredArtists :: AppState -> List UIName Artist
getFilteredArtists state = filterArtists ft (state^.artists)
  where ft = concat $ getEditContents (state^.filterEditor)

filterArtists :: Text -> List UIName Artist -> List UIName Artist
-- filterArtists ft as = list (UIName "filtered-artists") (fromList filtered) 1
filterArtists ft as = filtered
  where
    -- filtered = simpleFilter (unpack ft) (toList as)
    -- filteredList = fromString $ fromList filtered
    filtered = as & listElementsL %~ (\v -> toVec (simpleFilter (unpack ft) (fromVec v)))
    toVec l = fromList (fromString <$> l) :: Vector Artist
    fromVec v = toString <$> (toList (v :: Vector Artist)) :: [String]

event :: AppState -> BrickEvent UIName e -> EventM UIName (Next AppState)
event state (VtyEvent e) = case (state^.filterFocused) of
  True -> case e of
    (V.EvKey V.KEsc []) -> continue (state & filterActive .~ False
                                           & filterFocused .~ False
                                           & filterEditor %~ clearFilterEditor
                                           & filteredArtists .~ (filterArtists "" (state^.artists)))
    (V.EvKey V.KEnter []) -> continue (state & filterFocused .~ False)
    ev -> do
      newFilterEditor <- handleEditorEvent ev (state^.filterEditor)
      let newEditorState = state & filterEditor .~ newFilterEditor
      continue $ newEditorState & filteredArtists .~ (getFilteredArtists newEditorState)
  False -> case e of
    (V.EvKey (V.KChar '/') []) -> continue (state & filterActive .~ True
                                                  & filterFocused .~ True)
    (V.EvKey (V.KChar 'j') []) -> next state
    (V.EvKey (V.KChar 'k') []) -> previous state
    -- (V.EvKey V.KEnter []) -> play state
    (V.EvKey (V.KChar 'q') []) -> halt state
    (V.EvKey V.KEsc []) -> case (state^.filterActive) of
      True -> continue (state & filterActive .~ False
                              & filterEditor %~ clearFilterEditor
                              & filteredArtists .~ (filterArtists "" (state^.artists)))
      False -> continue state
    -- VtyEvent (V.EvKey (V.KChar '1') []) -> changeView state 1
    {-V.EvKey (V.KChar '-') [] -> delete-}
    ev -> listEvent ev state
event state _ = continue state

clearFilterEditor :: Editor Text UIName -> Editor Text UIName
clearFilterEditor editor = applyEdit (\_ -> textZipper [""] (Just 1)) editor

next :: AppState -> NextState
next state = continue $ state & filteredArtists %~ listMoveDown

previous :: AppState -> NextState
previous state = continue $ state & filteredArtists %~ listMoveUp

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
-- listEvent event state = continue =<< (\m -> state & artists .~ m) <$> handleListEvent event (state^.artists)
-- listEvent event state = continue =<< case (state^.filterActive) of
--   True -> (\m -> state & filteredArtists .~ m) <$> handleListEvent event (state^.filteredArtists)
--   False -> (\m -> state & artists .~ m) <$> handleListEvent event (state^.artists)
listEvent event state = continue =<< (\m -> state & filteredArtists .~ m) <$> handleListEvent event (state^.filteredArtists)


