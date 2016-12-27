{-# LANGUAGE OverloadedStrings #-}

module UI
( start
) where

import Control.Monad (forever)

import Data.Default (def)

import ClassyPrelude
import Data.List ((!!))
import Control.Monad (void)

import Config

-- import UI.Types (AppState(..), ViewType(..), playlist, activeView, helpActive, VtyEvent(..), UIName(..))
import UI.Types (AppState(..), ViewType(..), playlist, activeView, helpActive, UIName(..), artists, filteredArtists)
import qualified UI.Utils as Utils
import qualified UI.Views.Main as MainView
import qualified UI.Views.Playlist as PlaylistView
import qualified UI.Views.Library as LibraryView
import qualified UI.Views.Help as HelpView
import qualified UI.Widgets.Status as Status
import qualified UI.Widgets.Playlist as Playlist

import qualified Graphics.Vty as V

import qualified Brick.Main as M
import Brick.Types (EventM, Next(..), BrickEvent(..))
import Brick.Widgets.Core (str)
import Brick.Widgets.List (list)
import Brick.Widgets.Edit (editorText)
import Brick.AttrMap (AttrMap, attrMap)
import Brick.Types (Widget)

import Network.MPD (withMPD, playlistInfo, play, pause, Song(..), State(..), Status(..), Metadata(..), Artist, status)
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
initialState :: [Song] -> [Artist] -> AppState
initialState playlist artistsList = AppState
  -- { _mails = list (UIName "mails") (fromList mails) 1
  { _playlist = list (UIName "playlist") (fromList playlist) 1
  -- , _config = config
    -- Note: we have only one line so we can do str.head
  , _filterEditor = editorText (UIName "editor-fzf") (str . (concatMap unpack)) (Just 1) ""
  , _filterActive = False
  , _filterFocused = False
  -- , _activeView = PlaylistView
  , _artists = list (UIName "artists") (fromList artistsList) 1
  , _filteredArtists = list (UIName "artists-filtered") (fromList artistsList) 1
  , _activeView = LibraryView
  , _helpActive = False
  }

attributesMap :: AttrMap
attributesMap = attrMap V.defAttr $ concat
    [ Utils.attrs
    , Playlist.attrs
    , Status.attrs
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

-- newMailAction :: Chan VtyEvent -> IO ()
-- newMailAction chan = writeChan chan NewMailEvent

-- start :: Config -> (Account -> IO [Mail]) -> IO ()
-- start config update = do
-- start :: Config -> IO ()
-- start config = do
start :: IO ()
start = do
  -- let defaultAccount = (elems (accounts config)) !! 1
  -- mails <- update defaultAccount
  res <- withMPD $ playlistInfo Nothing
  case res of
    Left err -> print "Err reading initial playlist state"
    Right songs -> do
      artistsRes <- withMPD $ MPD.list Artist Nothing
      case artistsRes of
        Left err -> print $ "Error retrieving artists list: " ++ show err
        Right artistsList -> do
          let initState = initialState songs artistsList

          -- void $ M.customMain (V.mkVty def) chan app initState
          -- void $ M.defaultMain app initState
          void $ M.defaultMain app initState
