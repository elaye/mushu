{-# LANGUAGE TemplateHaskell #-}

module UI.Types
( AppState(..)
, ViewType(..)
, LibraryColumn(..)
, AppException(..)
, MPDEvent(..)
, activeView
, playlist
, helpActive
, filterActive
, filterFocused
, filterEditor
, library
, libraryArtists
, libraryAlbums
, librarySongs
, libraryActiveColumn
, filteredLibrary
, status
, UIName(..)
) where

import ClassyPrelude

import Control.Exception.Safe (Exception(..))

import Brick.Widgets.Edit (Editor(..))
import qualified Brick.Widgets.List as L
import Library

import Data.Map.Strict (Map(..))

import Lens.Micro.Platform (makeLenses)
import qualified Graphics.Vty as V

import qualified Network.MPD as M

data ViewType = PlaylistView | LibraryView

instance Show ViewType where
  show PlaylistView = "Playlist"
  show LibraryView = "Library"

data LibraryColumn = ArtistsColumn | AlbumsColumn | SongsColumn deriving (Show, Eq)

data AppState = AppState
  { _playlist :: L.List UIName M.Song
  , _filterEditor :: Editor Text UIName
  , _filterActive :: Bool
  , _filterFocused :: Bool
  , _activeView :: ViewType
  , _library :: Library
  , _filteredLibrary :: Library
  , _libraryArtists :: L.List UIName Text
  , _libraryAlbums :: L.List UIName Text
  , _librarySongs :: L.List UIName Text
  , _libraryActiveColumn :: LibraryColumn
  , _status :: M.Status
  , _helpActive :: Bool
  }

data UIName = UIName Text deriving (Show, Eq, Ord)

data AppException = MPDException | UnknownException deriving (Show, Typeable)

data MPDEvent = MPDPlaylistEvent
  | MPDDatabaseEvent
  | MPDStatusEvent
  | MPDUnknownEvent deriving (Show)

instance Exception AppException

instance IsString UIName where
  fromString s = UIName (pack s)

makeLenses ''AppState

