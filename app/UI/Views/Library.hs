{-# LANGUAGE GADTs #-}
module UI.Views.Library
( draw
, event
, attrs
) where

import ClassyPrelude hiding ((<>), on)
import Control.Exception.Safe (throw)
import Data.Monoid ((<>))
import Data.Map.Strict (elemAt)
import qualified Data.Set as Set
import qualified Data.Vector as V
import Data.Vector ((!), (!?), empty)

import qualified UI.Views.Main as Main
import Network.MPD (Song(..), Metadata(..), toString)
import MPD (tag, addToPlaylist)
import qualified MPD

import Brick.Types (Widget(..), Padding(..), BrickEvent(..), EventM, Next)
import Brick.Widgets.List
  ( List
  , list
  , renderList
  , listAttr
  , listSelectedAttr
  , listSelectedFocusedAttr
  , listMoveUp
  , listMoveDown
  , listSelectedL
  , listElementsL
  , listSelectedElement
  )
import Brick.Widgets.Core ((<+>), (<=>), str, withAttr, padLeft, padRight, hLimit)
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.Border (hBorder, vBorder)
import Brick.AttrMap (AttrName)
import Brick.Util (clamp, fg, on)
import Brick.Main (continue, halt)
import Lens.Micro.Platform ((^.), (%~), (&), (.~))

import qualified Graphics.Vty as Vty

import qualified UI.Widgets.Library as Library
-- import UI.Widgets.Library (albums, artistAlbums)
import qualified UI.Widgets.Filter as Filter
import UI.Widgets.Filter (isActiveL, isFocusedL)

import Library (Library(..), ArtistName, AlbumName, albumsL, artistsL)

import Types (AppException(..))
import UI.Types
import UI.Utils (listGetSelected)

import Text.Fuzzy (simpleFilter)

type NextState n = EventM n (Next (AppState n))

draw :: (Show n, Ord n) => AppState n -> [Widget n]
draw state = Main.draw state $ widget
  where
    fzf = Filter.mkWidget $ state^.filterStateL
    columns = column "Artists" True artistsWidget <+> column "Albums" True albumsWidget <+> column "Songs" False songsWidget
    column name bBorder widget = (title name) <=> if bBorder then (widget <+> vBorder) else widget
    title t = (padRight Max $ str t) <=> hBorder

    artistsWidget = columnWidget ArtistsColumn (state^.libraryArtists)
    albumsWidget = columnWidget AlbumsColumn (state^.libraryAlbums)
    songsWidget = columnWidget SongsColumn (state^.librarySongs)

    columnWidget column els = renderList (listDrawElement column activeColumn) False els
    activeColumn = state^.libraryActiveColumn
    widget = case (state^.filterStateL.isActiveL) of
      True -> fzf <=> hBorder <=> columns
      False -> columns

listDrawElement :: LibraryColumn -> LibraryColumn -> Bool -> Text -> Widget n
listDrawElement column activeColumn sel el = hCenter $ formatListElement column activeColumn sel $ pad $ str (unpack el)
  where
    pad w = padLeft Max $ padRight Max $ w

formatListElement :: LibraryColumn -> LibraryColumn -> Bool -> Widget n -> Widget n
formatListElement column activeColumn sel widget = withAttr attr widget
  where
    attr = case column == activeColumn of
          True -> case sel of
            True -> selActiveColAttrName
            False -> activeColAttrName
          False -> case columnIsBefore column activeColumn of
            True -> case sel of
              True -> listSelAttrName
              False -> listAttrName
            False -> listAttrName


-- TODO: find a better way to do this
-- column != activeColumn must be true before calling this function
columnIsBefore :: LibraryColumn -> LibraryColumn -> Bool
columnIsBefore column activeColumn = case activeColumn of
  ArtistsColumn -> False
  SongsColumn -> True
  AlbumsColumn -> case column of
    ArtistsColumn -> True
    SongsColumn -> False

listAttrName :: AttrName
listAttrName = listAttr <> "custom"

listSelAttrName :: AttrName
listSelAttrName = listSelectedAttr <> "custom-selected"

activeColAttrName :: AttrName
activeColAttrName = listAttrName <> "active-column"

selActiveColAttrName :: AttrName
selActiveColAttrName = listSelAttrName <> "selected-active-column"

attrs :: [(AttrName, Vty.Attr)]
attrs = [ (listAttrName, Vty.white `on` Vty.black)
        , (listSelAttrName, Vty.black `on` Vty.yellow)
        , (activeColAttrName, fg Vty.white)
        , (selActiveColAttrName, Vty.black `on` Vty.green)
        , (listSelectedFocusedAttr, fg Vty.red)
        ]

event :: AppState UIName -> BrickEvent UIName e -> EventM UIName (Next (AppState UIName))
event state (VtyEvent e) = case (state^.filterStateL.isFocusedL) of
  True -> case e of
    (Vty.EvKey Vty.KEsc []) -> continue $ resetFilter state
    (Vty.EvKey Vty.KEnter []) -> continue (state & filterStateL %~ Filter.blur)
    ev -> do
      newFilterState <- Filter.handleEvent ev $ state^.filterStateL
      continue $ applyFilter (state & filterStateL .~ newFilterState)
  False -> case e of
    (Vty.EvKey (Vty.KChar 'j') []) -> case activeColumn of
      ArtistsColumn -> nextArtist state
      AlbumsColumn -> nextAlbum state
      SongsColumn -> nextSong state
    (Vty.EvKey (Vty.KChar 'k') []) -> case activeColumn of
      ArtistsColumn -> previousArtist state
      AlbumsColumn -> previousAlbum state
      SongsColumn -> previousSong state
    (Vty.EvKey (Vty.KChar 'l') []) -> nextColumn state
    (Vty.EvKey (Vty.KChar 'h') []) -> previousColumn state
    (Vty.EvKey (Vty.KChar 'a') []) -> void (liftIO (addSelectedToPlaylist state)) >> continue state
    (Vty.EvKey Vty.KEnter []) -> void (liftIO (addToPlaylistAndPlay state)) >> continue state
    -- (Vty.EvKey Vty.KEnter []) -> play state
    (Vty.EvKey (Vty.KChar 'q') []) -> halt state
    (Vty.EvKey (Vty.KChar 'f') []) -> continue $ state & filterStateL %~ Filter.focus
    (Vty.EvKey Vty.KEsc []) -> case (state^.filterStateL.isActiveL) of
      True -> continue $ resetFilter state
      False -> continue state
    ev -> continue state
  where activeColumn = state^.libraryActiveColumn
event state _ = continue state

applyFilter :: AppState UIName -> AppState UIName
applyFilter state = state & libraryArtists .~ (list (UIName "filtered-artists") (V.fromList (getFilteredLibrary state)) 1)

resetFilter :: AppState UIName -> AppState UIName
resetFilter state = state & filterStateL %~ Filter.reset
                          & libraryArtists .~ artistsWidget
  where
    artistsWidget = list (UIName "filtered-artists") artists 1
    artists = V.fromList . keys $ state^.library.artistsL

getFilteredLibrary :: AppState n -> [ArtistName]
getFilteredLibrary state = filterLibrary ft (keys (state^.library.artistsL))
  where ft = Filter.getValue $ state^.filterStateL

filterLibrary :: Text -> [ArtistName] -> [ArtistName]
filterLibrary ft as = pack <$> filtered
  where filtered = simpleFilter (unpack ft) (unpack <$> as)

nextArtist :: AppState UIName -> NextState UIName
nextArtist state = continue $ updateSongs $ updateAlbums (state & libraryArtists %~ listMoveDown)

previousArtist :: AppState UIName -> NextState UIName
previousArtist state = continue $ updateSongs $ updateAlbums (state & libraryArtists %~ listMoveUp)

updateAlbums :: AppState UIName -> AppState UIName
updateAlbums state = state & libraryAlbums .~ (list (UIName "albums") newAlbums 1)
  where
    selArtist = snd <$> (listSelectedElement $ state^.libraryArtists)
    newAlbums = case selArtist of
      Just a -> fromMaybe V.empty ((V.fromList . Set.toAscList) <$> (lookup a (state^.filteredLibrary.artistsL)))
      Nothing -> V.empty

updateSongs :: AppState UIName -> AppState UIName
updateSongs state = state & librarySongs .~ (map (tag Title "<no title>") newSongsWidget)
  where
    selAlbum = snd <$> (listSelectedElement $ state^.libraryAlbums)
    selArtist = snd <$> (listSelectedElement $ state^.libraryArtists)
    newSongsWidget = list (UIName "songs") newSongsFiltered 1
    -- Songs are filtered by the selected artist if we are in artist/album/songs mode
    newSongsFiltered = case selArtist of
      Just a -> filter (\s -> tag Artist "" s == a) newSongs
      Nothing -> newSongs
    newSongs = case selAlbum of
      Just a -> fromMaybe V.empty (lookup a (state^.filteredLibrary.albumsL))
      Nothing -> V.empty

nextAlbum :: AppState UIName  -> NextState UIName
nextAlbum state = continue $ updateSongs $ state & libraryAlbums %~ listMoveDown

previousAlbum :: AppState UIName -> NextState UIName
previousAlbum state = continue $ updateSongs $ state & libraryAlbums %~ listMoveUp

nextSong :: AppState n -> NextState n
nextSong state = continue $ state & librarySongs %~ listMoveDown

previousSong :: AppState n -> NextState n
previousSong state = continue $ state & librarySongs %~ listMoveUp

nextColumn :: AppState n -> NextState n
nextColumn state = continue $ state & libraryActiveColumn .~ nextCol
  where
    nextCol = case (state^.libraryActiveColumn) of
                ArtistsColumn -> AlbumsColumn
                AlbumsColumn -> SongsColumn
                SongsColumn -> SongsColumn

previousColumn :: AppState n  -> NextState n
previousColumn state = continue $ state & libraryActiveColumn .~ prevCol
  where
    prevCol = case (state^.libraryActiveColumn) of
                ArtistsColumn -> ArtistsColumn
                AlbumsColumn -> ArtistsColumn
                SongsColumn -> AlbumsColumn

getSelected :: AppState n -> (Maybe Text, Maybe Text, Maybe Text)
getSelected state = (artist, album, title)
  where
    activeColumn = state^.libraryActiveColumn
    artist = listGetSelected $ state^.libraryArtists
    album = case activeColumn == AlbumsColumn of
      True -> listGetSelected $ state^.libraryAlbums
      False -> Nothing
    title = case activeColumn == SongsColumn of
      True -> listGetSelected $ state^.librarySongs
      False -> Nothing


addSelectedToPlaylist :: AppState n -> IO ()
addSelectedToPlaylist state = addToPlaylist $ getSelected state

addToPlaylistAndPlay :: AppState n -> IO ()
addToPlaylistAndPlay state = MPD.addToPlaylistAndPlay $ getSelected state

-- libraryEvent :: Vty.Event -> AppState -> NextState
-- libraryEvent event state = continue =<< (\m -> state & filteredLibrary .~ m) <$> handleLibraryEvent event (state^.library)
