{-# LANGUAGE TemplateHaskell #-}
module UI.Types
( AppState(..)
, ViewType(..)
-- , config
, activeView
, playlist
, helpActive
, VtyEvent(..)
, UIName(..)
) where

import ClassyPrelude

import qualified Brick.Widgets.List as L

import Config (Config)

import Lens.Micro.Platform (makeLenses)
import qualified Graphics.Vty as V

import Network.MPD (Song(..))

data ViewType = PlaylistView deriving Show

data AppState = AppState
  { _playlist :: L.List UIName Song
  -- , _config :: Config
  , _activeView :: ViewType
  , _helpActive :: Bool
  }

data VtyEvent = VtyEvent V.Event
  -- | NewMailEvent

data UIName = UIName Text deriving (Show, Eq, Ord)

makeLenses ''AppState
 
