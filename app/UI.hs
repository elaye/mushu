{-# LANGUAGE OverloadedStrings #-}

module UI
( start
) where

import ClassyPrelude
import Config
import Library
import UI.Types
-- import Library (Library, artistsL, albumsL, fetchLibrary)

import Control.Monad (void, forever)
import Lens.Micro.Platform ((^.), (&), (.~), (%~))

import Data.Default (def)
import Data.Map.Strict (elemAt)
import Data.List ((!!))
import Data.HashMap.Strict (elems)
import qualified Data.Vector as V

-- import UI.Types (AppState(..), ViewType(..), ActiveColumn(..), playlist, activeView, helpActive, UIName(..), library, filteredLibrary, libraryActiveColumn, filterFocused)
import qualified UI.Utils as Utils
import qualified UI.Views.Main as MainView
import qualified UI.Views.Playlist as PlaylistView
import qualified UI.Views.Library as LibraryView
import qualified UI.Views.Help as HelpView
import qualified UI.Widgets.Status as Status
import qualified UI.Widgets.Playlist as Playlist

import Brick.Types (Widget, EventM, Next(..), BrickEvent(..))
import Brick.Widgets.Core (str)
import Brick.Widgets.List (list)
import Brick.Widgets.Edit (editorText)
import Brick.AttrMap (AttrMap, attrMap)
import qualified Brick.Main as M

import qualified Graphics.Vty as Vty

import qualified Network.MPD as MPD
import Network.MPD
  ( withMPD
  , playlistInfo
  , Song(..)
  , Metadata(..)
  , Artist
  )

import MPD (togglePlayPause)

type NextState = EventM UIName (Next AppState)

drawUI :: AppState -> [Widget UIName]
drawUI state = if state^.helpActive
  then HelpView.draw
  else case state^.activeView of
    PlaylistView -> PlaylistView.draw state
    LibraryView -> LibraryView.draw state

appEvent :: AppState -> BrickEvent UIName e -> NextState
appEvent state event = case state^.filterFocused of
  True -> handleViewEvent state event
  False -> case event of
    VtyEvent (Vty.EvKey (Vty.KChar '?') []) -> M.continue $ state & helpActive %~ not
    VtyEvent (Vty.EvKey (Vty.KChar 'p') []) -> void (liftIO togglePlayPause) >> M.continue state
    VtyEvent (Vty.EvKey (Vty.KChar '1') []) -> M.continue $ state & activeView .~ PlaylistView
    VtyEvent (Vty.EvKey (Vty.KChar '2') []) -> M.continue $ state & activeView .~ LibraryView
    ev -> handleViewEvent state ev

handleViewEvent :: AppState -> BrickEvent UIName e -> NextState
handleViewEvent state event = case state^.activeView of
  PlaylistView -> PlaylistView.event state event
  LibraryView -> LibraryView.event state event

-- toggle :: AppState -> NextState
-- toggle state = do
--   resStatus <- liftIO $ withMPD status
--   case resStatus of
--     Left _ -> M.continue state
--     Right st -> do
--       case (stState st) of
--         Playing -> do
--           _ <- liftIO $ withMPD $ pause True
--           M.continue state
--         Stopped -> do
--           _ <- liftIO $ withMPD $ play Nothing
--           M.continue state
--         Paused -> do
--           _ <- liftIO $ withMPD $ pause False
--           M.continue state

updatePlaylist :: AppState -> NextState
updatePlaylist state = do
  res <- liftIO $ withMPD $ playlistInfo Nothing
  case res of
    Left err -> M.continue state
    Right songs -> M.continue $ state & playlist .~ (list (UIName "playlist") (fromList songs) 1)

initialState :: [Song] -> Library -> AppState
initialState playlist library = AppState
  { _playlist = list (UIName "playlist") (fromList playlist) 1
  , _filterEditor = editorText (UIName "editor-fzf") (str . (concatMap unpack)) (Just 1) ""
  , _filterActive = False
  , _filterFocused = False
  , _activeView = LibraryView
  , _library = library
  , _filteredLibrary = library
  , _libraryArtists = list (UIName "artists") (fromList (keys artists)) 1
  , _libraryAlbums = list (UIName "albums") firstArtistAlbums 1
  , _librarySongs = list (UIName "songs") V.empty 1
  , _libraryActiveColumn = ArtistsColumn
  , _helpActive = False
  }
  where
    artists = library^.artistsL
    firstArtistAlbums = snd $ elemAt 0 artists

attributesMap :: AttrMap
attributesMap = attrMap Vty.defAttr $ concat
    [ Utils.attrs
    , Playlist.attrs
    , Status.attrs
    , LibraryView.attrs
    ]

app :: M.App AppState e UIName
app =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const attributesMap
          -- , M.appLiftVtyEvent = VtyEvent
          }

getPlaylist :: IO [Song]
getPlaylist = do
  res <- withMPD $ playlistInfo Nothing
  case res of
    Left err -> print "Err while getting list of songs" >> return []
    Right songs -> return songs

start :: IO ()
start = do
  playlist <- getPlaylist
  library <- fetchLibrary
  let initState = initialState playlist library
  void $ M.defaultMain app initState
