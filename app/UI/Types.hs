{-# LANGUAGE TemplateHaskell #-}

module UI.Types
( AppState(..)
, ViewType(..)
-- , config
, activeView
, playlist
, artists
, helpActive
, filterActive
, filterFocused
, filterEditor
, filteredArtists
, VtyEvent(..)
, UIName(..)
) where

import ClassyPrelude

import Brick.Widgets.Edit (Editor(..))
import qualified Brick.Widgets.List as L

import Config (Config)

import Lens.Micro.Platform (makeLenses)
import qualified Graphics.Vty as V

-- import Network.MPD (Song(..), Artist)
import qualified Network.MPD as M

data ViewType = PlaylistView | LibraryView deriving Show

data AppState = AppState
  { _playlist :: L.List UIName M.Song
  -- , _config :: Config
  , _filterEditor :: Editor Text UIName
  , _filterActive :: Bool
  , _filterFocused :: Bool
  , _activeView :: ViewType
  , _artists :: L.List UIName M.Artist
  -- , _library :: Library
  , _filteredArtists :: L.List UIName M.Artist
  , _helpActive :: Bool
  }

data VtyEvent = VtyEvent V.Event
  -- | NewMailEvent

data UIName = UIName Text deriving (Show, Eq, Ord)

makeLenses ''AppState
 
