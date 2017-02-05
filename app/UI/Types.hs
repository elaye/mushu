{-# LANGUAGE TemplateHaskell #-}

module UI.Types
( AppState(..)
, ViewType(..)
, LibraryColumn(..)
, MPDEvent(..)
, activeView
, playlist
, helpActive
, filterStateL
-- , filterActive
-- , filterFocused
-- , filterEditor
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

-- import Brick.Widgets.Edit (Editor(..))
import qualified Brick.Widgets.List as L
import Library

import UI.Widgets.Filter

import Data.Map.Strict (Map(..))

import Lens.Micro.Platform (makeLenses)
import qualified Graphics.Vty as V

import qualified Network.MPD as M

data ViewType = PlaylistView | LibraryView

instance Show ViewType where
  show PlaylistView = "Playlist"
  show LibraryView = "Library"

data LibraryColumn = ArtistsColumn | AlbumsColumn | SongsColumn deriving (Show, Eq)


data AppState n = AppState
  { _playlist :: L.List n M.Song
  -- , _filterEditor :: Editor Text UIName
  -- , _filterActive :: Bool
  -- , _filterFocused :: Bool
  , _filterStateL :: FilterState n
  , _activeView :: ViewType
  , _library :: Library
  , _filteredLibrary :: Library
  , _libraryArtists :: L.List n Text
  , _libraryAlbums :: L.List n Text
  , _librarySongs :: L.List n Text
  , _libraryActiveColumn :: LibraryColumn
  , _status :: M.Status
  , _helpActive :: Bool
  }

data UIName = UIName Text deriving (Show, Eq, Ord)

data MPDEvent = MPDPlaylistEvent
  | MPDDatabaseEvent
  | MPDStatusEvent
  | MPDUnknownEvent deriving (Show)


instance IsString UIName where
  fromString s = UIName (pack s)

makeLenses ''AppState

