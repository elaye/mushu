{-# LANGUAGE TemplateHaskell #-}

module UI.Types
( AppState(..)
, ViewType(..)
, ActiveColumn(..)
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
, UIName(..)
) where

import ClassyPrelude

import Brick.Widgets.Edit (Editor(..))
import qualified Brick.Widgets.List as L
import Library

import Data.Map.Strict (Map(..))

import Lens.Micro.Platform (makeLenses)
import qualified Graphics.Vty as V

import qualified Network.MPD as M

data ViewType = PlaylistView | LibraryView deriving Show

data ActiveColumn = ArtistsColumn | AlbumsColumn | SongsColumn deriving (Show, Eq)

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
  , _libraryActiveColumn :: ActiveColumn
  , _helpActive :: Bool
  }

data UIName = UIName Text deriving (Show, Eq, Ord)

instance IsString UIName where
  fromString s = UIName (pack s)

makeLenses ''AppState

