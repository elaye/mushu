module UI
( start
) where

import ClassyPrelude
import Library
import UI.Types

import Control.Monad (void, forever)
import Lens.Micro.Platform ((^.), (&), (.~), (%~))

import Data.Default (def)
import Data.Map.Strict (elemAt)
import qualified Data.Set as Set
import Data.List ((!!))
import Data.HashMap.Strict (elems)
import qualified Data.Vector as V

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

import Network.MPD (Song)
import MPD (togglePlayPause, fetchPlaylist)

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

updatePlaylist :: AppState -> NextState
updatePlaylist state = do
  songs <- liftIO $ fetchPlaylist
  let playlistWidget = list (UIName "playlist") (fromList songs) 1
  M.continue $ state & playlist .~ playlistWidget

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
    firstArtistAlbums = V.fromList . Set.toAscList $ snd $ elemAt 0 artists

attributesMap :: AttrMap
attributesMap = attrMap Vty.defAttr $ concat
  [ Utils.attrs
  , Playlist.attrs
  , Status.attrs
  , LibraryView.attrs
  ]

app :: M.App AppState e UIName
app = M.App
  { M.appDraw = drawUI
  , M.appChooseCursor = M.showFirstCursor
  , M.appHandleEvent = appEvent
  , M.appStartEvent = return
  , M.appAttrMap = const attributesMap
  -- , M.appLiftVtyEvent = VtyEvent
  }

start :: IO ()
start = do
  playlist <- fetchPlaylist
  library <- fetchLibrary
  let initState = initialState playlist library
  void $ M.defaultMain app initState
