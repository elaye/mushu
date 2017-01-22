{-# LANGUAGE OverloadedStrings #-}
module UI.Views.Library
( draw
, event
) where

import ClassyPrelude hiding ((<>))
import Data.Monoid ((<>))
import Data.Map.Strict (elemAt)
import Data.Vector ((!))

import qualified UI.Views.Main as Main

import Brick.Types (Widget(..), Padding(..), BrickEvent(..), EventM, Next)
import Brick.Widgets.List
  ( List
  , list
  , renderList
  , listSelectedAttr
  , listAttr
  , listMoveUp
  , listMoveDown
  , listSelectedL
  , listElementsL
  )
import Brick.Widgets.Core ((<+>), str, withAttr, padLeft, padRight, hLimit)
import Brick.Widgets.Center (hCenter)
import Brick.AttrMap (AttrName)
import Brick.Util (clamp)
import Brick.Main (continue, halt)
import Lens.Micro.Platform ((^.), (%~), (&), (.~))

import qualified Graphics.Vty as V

import qualified UI.Widgets.Library as Library
import UI.Widgets.Library (albums, artistAlbums)

import UI.Types (AppState(..), UIName(..), library, filteredLibrary, libraryArtists, libraryAlbums, librarySongs)

type NextState = EventM UIName (Next AppState)

draw :: AppState -> [Widget UIName]
draw state = Main.draw state $ artistsWidget <+> albumsWidget <+> songsWidget
  -- where widget = Library.mkWidget (state^.filteredLibrary) 
  where
    artistsWidget = renderList listDrawElement True (state^.libraryArtists)
    albumsWidget = renderList listDrawElement True (state^.libraryAlbums)
    songsWidget = renderList listDrawElement True (state^.librarySongs)

listDrawElement :: Bool -> Text -> Widget UIName
listDrawElement sel el = hCenter $ formatListElement sel False $ pad $ str (unpack el)
  where
    pad w = padLeft Max $ padRight Max $ w

formatListElement :: Bool -> Bool -> Widget UIName -> Widget UIName
formatListElement playing sel widget = withAttr attr widget
  where attr = case playing of
                True -> case sel of
                  True -> selPlayingAttrName
                  False -> playingAttrName
                False -> case sel of
                  True -> selAttrName
                  False -> listAttr

selAttrName :: AttrName
selAttrName = listSelectedAttr <> "custom"

newAttrName :: AttrName
newAttrName = listAttr <> "new"

playingAttrName :: AttrName
playingAttrName = listAttr <> "playing"

selPlayingAttrName :: AttrName
selPlayingAttrName = listAttr <> "selected-playing"

event :: AppState -> BrickEvent UIName e -> EventM UIName (Next AppState)
event state (VtyEvent e) = case e of
    (V.EvKey (V.KChar 'j') []) -> next state
    (V.EvKey (V.KChar 'k') []) -> previous state
    -- (V.EvKey V.KEnter []) -> play state
    (V.EvKey (V.KChar 'q') []) -> halt state
    -- VtyEvent (V.EvKey (V.KChar '1') []) -> changeView state 1
    {-V.EvKey (V.KChar '-') [] -> delete-}
    -- ev -> listEvent ev state
    ev -> continue state
event state _ = continue state

next :: AppState -> NextState
next state = continue $ state
  & libraryArtists %~ listMoveDown
  & libraryAlbums .~ albs
  where
    arts = state^.libraryArtists.listElementsL
    len = length arts
    i = fromMaybe 0 (state^.libraryArtists.listSelectedL)
    j = clamp 0 (len - 1) (i + 1)
    nextArtist =  arts ! j
    lib = state^.filteredLibrary.artistAlbums
    albs = list (UIName "albums") (fromList (maybe [] (\a -> keys (a^.albums)) (lookup nextArtist lib))) 1

previous :: AppState -> NextState
previous state = continue $ state
  & libraryArtists %~ listMoveUp
  & libraryAlbums .~ albs
  where
    arts = state^.libraryArtists.listElementsL
    len = length arts
    i = fromMaybe 0 (state^.libraryArtists.listSelectedL)
    j = clamp 0 (len - 1) (i - 1)
    nextArtist =  arts ! j
    lib = state^.filteredLibrary.artistAlbums
    albs = list (UIName "albums") (fromList (maybe [] (\a -> keys (a^.albums)) (lookup nextArtist lib))) 1

-- next :: AppState -> NextState
-- next state = continue $ state & filteredLibrary %~ Library.libraryMoveDown

-- previous :: AppState -> NextState
-- previous state = continue $ state & filteredLibrary %~ Library.libraryMoveUp

-- libraryEvent :: V.Event -> AppState -> NextState
-- libraryEvent event state = continue =<< (\m -> state & filteredLibrary .~ m) <$> handleLibraryEvent event (state^.library)

-- libraryEvent :: V.Event -> AppState -> NextState
-- libraryEvent event state = continue =<< (\m -> state & filteredLibrary .~ m) <$> handleLibraryEvent event (state^.library)
