{-# LANGUAGE TemplateHaskell #-}

module UI.Types
( AppState(..)
, ViewType(..)
, ActiveColumn(..)
-- , config
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
, VtyEvent(..)
, UIName(..)
) where

import ClassyPrelude

import Brick.Widgets.Edit (Editor(..))
import qualified Brick.Widgets.List as L
import UI.Widgets.Library (Library)

import Config (Config)

import Lens.Micro.Platform (makeLenses)
import qualified Graphics.Vty as V

-- import Network.MPD (Song(..), Artist)
import qualified Network.MPD as M

data ViewType = PlaylistView | LibraryView deriving Show

data ActiveColumn = ArtistsColumn | AlbumsColumn | SongsColumn deriving (Show, Eq)

data AppState = AppState
  { _playlist :: L.List UIName M.Song
  -- , _config :: Config
  , _filterEditor :: Editor Text UIName
  , _filterActive :: Bool
  , _filterFocused :: Bool
  , _activeView :: ViewType
  -- , _artists :: L.List UIName M.Artist
  , _library :: Library UIName
  , _filteredLibrary :: Library UIName
  , _libraryArtists :: L.List UIName Text
  , _libraryAlbums :: L.List UIName Text
  , _librarySongs :: L.List UIName Text
  , _libraryActiveColumn :: ActiveColumn
  -- , _filteredArtists :: L.List UIName M.Artist
  , _helpActive :: Bool
  }

data VtyEvent = VtyEvent V.Event
  -- | NewMailEvent

data UIName = UIName Text deriving (Show, Eq, Ord)

instance IsString UIName where
  fromString s = UIName (pack s)

makeLenses ''AppState
 
