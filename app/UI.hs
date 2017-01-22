{-# LANGUAGE OverloadedStrings #-}

module UI
( start
) where

import Control.Monad (forever)

import Data.Default (def)
import Data.Map.Strict (elemAt)

import ClassyPrelude
import Data.List ((!!))
import Control.Monad (void)

import Config

-- import UI.Types (AppState(..), ViewType(..), playlist, activeView, helpActive, VtyEvent(..), UIName(..))
-- import UI.Types (AppState(..), ViewType(..), playlist, activeView, helpActive, UIName(..), artists, filteredArtists)
import UI.Types (AppState(..), ViewType(..), ActiveColumn(..), playlist, activeView, helpActive, UIName(..), library, filteredLibrary, libraryActiveColumn)
import qualified UI.Utils as Utils
import qualified UI.Views.Main as MainView
import qualified UI.Views.Playlist as PlaylistView
import qualified UI.Views.Library as LibraryView
import qualified UI.Views.Help as HelpView
import qualified UI.Widgets.Status as Status
import qualified UI.Widgets.Playlist as Playlist
import qualified UI.Widgets.Library as Library
import UI.Widgets.Library (artistAlbums, albums)
import UI.Widgets.Library (Library(..))

import qualified Graphics.Vty as V

import qualified Brick.Main as M
import Brick.Types (EventM, Next(..), BrickEvent(..))
import Brick.Widgets.Core (str)
import Brick.Widgets.List (list)
import Brick.Widgets.Edit (editorText)
import Brick.AttrMap (AttrMap, attrMap)
import Brick.Types (Widget)

import UI.Widgets.Library (fetchLibrary)

import Network.MPD
  ( withMPD
  , playlistInfo
  , play
  , pause
  , Song(..)
  , State(..)
  , Status(..)
  , Metadata(..)
  , Artist
  , status
  )

import qualified Network.MPD as MPD

import Data.HashMap.Strict (elems)
import Lens.Micro.Platform ((^.), (&), (.~), (%~))

type NextState = EventM UIName (Next AppState)

{-drawUI :: (Show a) => L.List () a -> [Widget ()]-}
drawUI :: AppState -> [Widget UIName]
drawUI state = if state^.helpActive
  then HelpView.draw
  else case state^.activeView of
    PlaylistView -> PlaylistView.draw state
    LibraryView -> LibraryView.draw state

-- appEvent :: AppState -> BrickEvent UIName VtyEvent -> NextState
appEvent :: AppState -> BrickEvent UIName e -> NextState
appEvent state event = case event of
    VtyEvent (V.EvKey (V.KChar '?') []) -> M.continue $ state & helpActive %~ not
    VtyEvent (V.EvKey (V.KChar 'p') []) -> toggle state
    -- NewMailEvent -> updateMails state
    ev -> case state^.activeView of
      PlaylistView -> PlaylistView.event state ev
      LibraryView -> LibraryView.event state ev


toggle :: AppState -> NextState
toggle state = do
  resStatus <- liftIO $ withMPD status
  case resStatus of
    Left _ -> M.continue state
    Right st -> do
      case (stState st) of
        Playing -> do
          _ <- liftIO $ withMPD $ pause True
          M.continue state
        Stopped -> do
          _ <- liftIO $ withMPD $ play Nothing
          M.continue state
        Paused -> do
          _ <- liftIO $ withMPD $ pause False
          M.continue state

updatePlaylist :: AppState -> NextState
updatePlaylist state = do
  -- currentMails <- liftIO $ open (state^.activeAccount)
  -- M.continue $ state & mails .~ (list (UIName "mails") (fromList currentMails) 1)
  res <- liftIO $ withMPD $ playlistInfo Nothing
  case res of
    Left err -> M.continue state
    Right songs -> M.continue $ state & playlist .~ (list (UIName "playlist") (fromList songs) 1)

-- initialState :: Config -> [Song] -> AppState
-- initialState config playlist = AppState
-- initialState :: [Song] -> [Artist] -> AppState
-- initialState playlist artistsList = AppState
initialState :: [Song] -> Library UIName -> AppState
initialState playlist library = AppState
  -- { _mails = list (UIName "mails") (fromList mails) 1
  { _playlist = list (UIName "playlist") (fromList playlist) 1
  -- , _config = config
    -- Note: we have only one line so we can do str.head
  , _filterEditor = editorText (UIName "editor-fzf") (str . (concatMap unpack)) (Just 1) ""
  , _filterActive = False
  , _filterFocused = False
  -- , _activeView = PlaylistView
  -- , _artists = list (UIName "artists") (fromList artistsList) 1
  -- , _filteredArtists = list (UIName "artists-filtered") (fromList artistsList) 1
  , _activeView = LibraryView
  , _library = library
  , _filteredLibrary = library
  , _libraryArtists = list (UIName "artists") (fromList (keys aas)) 1
  , _libraryAlbums = list (UIName "albums") (fromList (keys ((snd (elemAt 0 aas))^.albums))) 1
  , _librarySongs = list (UIName "songs") (fromList []) 1
  , _libraryActiveColumn = ArtistsColumn
  , _helpActive = False
  }
  where
    aas = library^.artistAlbums

attributesMap :: AttrMap
attributesMap = attrMap V.defAttr $ concat
    [ Utils.attrs
    , Playlist.attrs
    , Status.attrs
    , Library.attrs
    , LibraryView.attrs
    -- , Explorer.attrs
    ]

-- app :: M.App AppState VtyEvent UIName
app :: M.App AppState e UIName
app =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const attributesMap
          -- , M.appLiftVtyEvent = VtyEvent
          }

-- getAllSongs :: IO [Song]
-- getAllSongs = do
--   res <- withMPD $ MPD.listAllInfo (fromString "")
--   let f x = case x of
--               LsSong _ -> True
--               _ -> False
--   case res of
--     Left err -> print "Err while getting list of songs" >> return []
--     Right lsRes -> return $ filter f lsRes

getPlaylist :: IO [Song]
getPlaylist = do
  res <- withMPD $ playlistInfo Nothing
  case res of
    Left err -> print "Err while getting list of songs" >> return []
    Right songs -> return songs

getArtists :: IO [Artist]
getArtists = do
  res <- withMPD $ MPD.list Artist Nothing
  case res of
    Left err -> print "Err while retrieving artists list" >> return []
    Right artists -> return artists

start :: IO ()
start = do
  playlist <- getPlaylist
  -- songs <- getAllSongs
  artists <- getArtists
  library <- fetchLibrary (UIName "library")
  -- print library
  -- let initState = initialState playlist songs
  -- let initState = initialState playlist artists
  let initState = initialState playlist library
  void $ M.defaultMain app initState
