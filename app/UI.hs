module UI
( start
) where

import ClassyPrelude hiding (catch)
import Library
import UI.Types
import Control.Exception.Safe (catch)

import Control.Concurrent.Async (async)

import Control.Monad (void, forever)
import Lens.Micro.Platform ((^.), (&), (.~), (%~))

import Data.Default (def)
-- import Data.Map.Strict (elemAt)
-- import qualified Data.Set as Set
import Data.List ((!!))
import Data.HashMap.Strict (elems)
-- import qualified Data.Vector as V

import qualified UI.Utils as Utils
import qualified UI.Views.Main as MainView
import qualified UI.Views.Playlist as PlaylistView
import qualified UI.Views.Library as LibraryView
import qualified UI.Views.Help as HelpView
import qualified UI.Widgets.Status as Status
import qualified UI.Widgets.Playlist as Playlist
import qualified UI.Widgets.Filter as Filter
import qualified UI.Widgets.Notification as Notification
import qualified UI.Widgets.Library as LibraryWidget
import UI.Widgets.Filter (isFocusedL)

import Brick.Types (Widget, EventM, Next(..), BrickEvent(..))
import Brick.Widgets.Core (str)
import Brick.Widgets.List (list)
import Brick.AttrMap (AttrMap, attrMap)
import Brick.BChan (BChan(..), newBChan, writeBChan)
import Brick.Util (clamp)
import qualified Brick.Main as M

import qualified Graphics.Vty as Vty
import Graphics.Vty.Config (defaultConfig)

import Network.MPD (Song, idle, Subsystem(..), Status(..))
import MPD (togglePlayPause, fetchPlaylist, fetchStatus, clearPlaylist, mpdReq, setVolume)

type NextState n = EventM n (Next (AppState n))

drawUI :: (Show n, Ord n) => AppState n -> [Widget n]
drawUI state = if state^.helpActive
  then HelpView.draw
  else case state^.activeView of
    PlaylistView -> PlaylistView.draw state
    LibraryView -> LibraryView.draw state

appEvent :: AppState UIName -> BrickEvent UIName MPDEvent -> NextState UIName
appEvent state event = case event of
  AppEvent MPDPlaylistEvent -> updatePlaylist state
  AppEvent MPDStatusEvent -> updateStatus state
  vtyEvent -> case (state^.filterStateL.isFocusedL) of
    -- True -> liftM catch (handleViewEvent state event) (\e -> (liftIO $ print "Exception") >> M.continue state)
    True -> handleViewEvent state vtyEvent
    False -> case vtyEvent of
      VtyEvent (Vty.EvKey (Vty.KChar '?') []) -> M.continue $ state & helpActive %~ not
      VtyEvent (Vty.EvKey (Vty.KChar 'p') []) -> void (liftIO togglePlayPause) >> M.continue state
      VtyEvent (Vty.EvKey (Vty.KChar 'c') []) -> void (liftIO clearPlaylist) >> M.continue state
      VtyEvent (Vty.EvKey (Vty.KChar '1') []) -> M.continue $ state & activeView .~ PlaylistView
      VtyEvent (Vty.EvKey (Vty.KChar '2') []) -> M.continue $ state & activeView .~ LibraryView
      VtyEvent (Vty.EvKey (Vty.KChar '-') []) -> void (liftIO (decreaseVolume state)) >> M.continue state
      VtyEvent (Vty.EvKey (Vty.KChar '+') []) -> void (liftIO (increaseVolume state)) >> M.continue state
      ev -> handleViewEvent state ev

handleViewEvent :: AppState UIName -> BrickEvent UIName e -> NextState UIName
handleViewEvent state event = case state^.activeView of
  PlaylistView -> PlaylistView.event state event
  LibraryView -> LibraryView.event state event

updatePlaylist :: AppState UIName -> NextState UIName
updatePlaylist state = do
  songs <- liftIO $ fetchPlaylist
  -- let playlistWidget = list (UIName "playlist") (fromList songs) 1
  M.continue $ state & playlistStateL %~ (Playlist.update songs)

updateStatus :: AppState n -> NextState n
updateStatus state = do
  st <- liftIO $ fetchStatus
  M.continue $ state & status .~ st

increaseVolume :: AppState n -> IO ()
increaseVolume state = changeVolume (+3) state

decreaseVolume :: AppState n -> IO ()
decreaseVolume state = changeVolume (\i -> i-3) state

changeVolume :: (Int -> Int) -> AppState n -> IO ()
changeVolume f state = case volume of
  Nothing -> return ()
  Just v -> setVolume $ clamp (f v) 0 100
  where volume = (stVolume (state^.status))

initialState :: [Song] -> Library -> Status -> AppState UIName
initialState playlist library status = AppState
  -- { _playlist = list (UIName "playlist") (fromList playlist) 1
  { _playlistStateL = Playlist.mkState (UIName "playlist") playlist
  , _filterStateL = Filter.mkState (UIName "filter")
  , _activeView = LibraryView
  , _libraryStateL = LibraryWidget.mkState (UIName "artists") (UIName "albums") (UIName "songs") library
  -- , _library = library
  -- , _filteredLibrary = library
  -- , _libraryArtists = list (UIName "artists") (fromList (keys artists)) 1
  -- , _libraryAlbums = list (UIName "albums") firstArtistAlbums 1
  -- , _librarySongs = list (UIName "songs") V.empty 1
  -- , _libraryActiveColumn = ArtistsColumn
  -- , _libraryMode = ArtistsAlbumsSongsMode
  , _status = status
  , _helpActive = False
  , _notificationState = Notification.mkState
  }
  -- where
  --   artists = library^.artistsL
  --   firstArtistAlbums = V.fromList . Set.toAscList $ snd $ elemAt 0 artists

attributesMap :: AttrMap
attributesMap = attrMap Vty.defAttr $ concat
  [ Utils.attrs
  , Playlist.attrs
  , Status.attrs
  , LibraryWidget.attrs
  , Filter.attrs
  ]

app :: M.App (AppState UIName) MPDEvent UIName
app = M.App
  { M.appDraw = drawUI
  , M.appChooseCursor = M.showFirstCursor
  , M.appHandleEvent = appEvent
  , M.appStartEvent = return
  , M.appAttrMap = const attributesMap
  }

toEvent :: Subsystem -> MPDEvent
toEvent UpdateS = MPDDatabaseEvent
toEvent PlaylistS = MPDPlaylistEvent
toEvent PlayerS = MPDStatusEvent
toEvent OptionsS = MPDStatusEvent
toEvent MixerS = MPDStatusEvent
toEvent _ = MPDUnknownEvent

mpdLoop :: BChan MPDEvent -> IO ()
mpdLoop chan = forever $ do
  subsChanges <- mpdReq $ idle [UpdateS, PlaylistS, OptionsS, PlayerS, MixerS]
  mapM_ (writeBChan chan) (map toEvent subsChanges)

start :: IO ()
start = do
  playlist <- fetchPlaylist
  library <- fetchLibrary
  status <- fetchStatus
  mpdEventChan <- newBChan 10
  let initState = initialState playlist library status
  async $ mpdLoop mpdEventChan
  void $ M.customMain (Vty.mkVty defaultConfig)
                    (Just mpdEventChan) app initState
