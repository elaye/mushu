{-# LANGUAGE TemplateHaskell #-}

module UI.Types
( AppState(..)
, ViewType(..)
-- , LibraryColumn(..)
, MPDEvent(..)
, UIName(..)
-- , LibraryMode(..)
, activeView
, playlistStateL
, helpActive
, filterStateL
, libraryStateL
-- , library
-- , libraryArtists
-- , libraryAlbums
-- , librarySongs
-- , libraryActiveColumn
-- , libraryMode
-- , filteredLibrary
, notificationState
, status
) where

import ClassyPrelude

import qualified Brick.Widgets.List as L
-- import Library

import UI.Widgets.Filter
import UI.Widgets.Notification
import UI.Widgets.Library
import UI.Widgets.Playlist

-- import Data.Map.Strict (Map(..))

import Lens.Micro.Platform (makeLenses)
-- import qualified Graphics.Vty as V

import qualified Network.MPD as M

data ViewType = PlaylistView | LibraryView

instance Show ViewType where
  show PlaylistView = "Playlist"
  show LibraryView = "Library"

-- data LibraryColumn = ArtistsColumn | AlbumsColumn | SongsColumn deriving (Show, Eq)
-- data LibraryMode = ArtistsAlbumsSongsMode | AlbumsSongsMode | SongsMode deriving (Show, Eq)

data AppState n = AppState
  { _playlistStateL :: PlaylistState n
  , _filterStateL :: FilterState n
  , _activeView :: ViewType
  , _libraryStateL :: LibraryState n
  -- , _library :: Library
  -- , _filteredLibrary :: Library
  -- , _libraryArtists :: L.List n Text
  -- , _libraryAlbums :: L.List n Text
  -- , _librarySongs :: L.List n Text
  -- , _libraryActiveColumn :: LibraryColumn
  -- , _libraryMode :: LibraryMode
  , _status :: M.Status
  , _helpActive :: Bool
  , _notificationState :: NotificationState
  }

data UIName = UIName Text deriving (Show, Eq, Ord)

data MPDEvent = MPDPlaylistEvent
  | MPDDatabaseEvent
  | MPDStatusEvent
  | MPDUnknownEvent deriving (Show)

instance IsString UIName where
  fromString s = UIName (pack s)

makeLenses ''AppState

