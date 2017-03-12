{-# LANGUAGE TemplateHaskell #-}
module UI.Widgets.Library
( mkState
, mkWidget
, LibraryState
, handleEvent
, applyFilter
, resetFilter
, attrs
) where

import ClassyPrelude hiding ((<>), on)
import Data.Monoid ((<>))

import TH (makeSuffixLenses)
import Lens.Micro.Platform ((^.), (&), (.~), (%~))

import Brick.Widgets.Core ((<+>), (<=>), str, padLeft, padRight, withAttr)
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.Border (vBorder, hBorder)
import Brick.Types (EventM, Widget, Padding(..))
import Brick.AttrMap (AttrName)
import Brick.Widgets.List (List(..), list, listMoveDown, listMoveUp, listReplace, listSelectedElement, renderList, listAttr, listSelectedAttr, listSelectedFocusedAttr)
import Brick.Util (fg, on)
import qualified Graphics.Vty as Vty

import Data.Map.Strict (elemAt)
import qualified Data.Vector as V
import qualified Data.Set as Set

import Network.MPD (Metadata(..))
import MPD (tag, addToPlaylist)
import qualified MPD
import UI.Utils (listGetSelected)

import Library

data LibraryColumn = ArtistsColumn | AlbumsColumn | SongsColumn deriving (Show, Eq)
data LibraryMode = ArtistsAlbumsSongsMode | AlbumsSongsMode deriving (Show, Eq)

data LibraryState n = LibraryState
  { _library :: Library
  , _filteredLibrary :: Library
  , _libraryArtists :: List n Text
  , _libraryAlbums :: List n Text
  , _librarySongs :: List n Text
  , _libraryActiveColumn :: LibraryColumn
  , _libraryMode :: LibraryMode
  , _libraryFilter :: Maybe ([Text] -> [Text])
  }

makeSuffixLenses ''LibraryState

mkWidget :: (Show n, Ord n) => LibraryState n -> Widget n
mkWidget state = columns
  where
    columns = case state^.libraryModeL of
      ArtistsAlbumsSongsMode -> column "Artists" True artistsWidget <+> column "Albums" True albumsWidget <+> column "Songs" False songsWidget
      AlbumsSongsMode -> column "Albums" True albumsWidget <+> column "Songs" False songsWidget
      -- SongsMode -> column "Songs" False songsWidget

    column name bBorder widget = title name <=> if bBorder then widget <+> vBorder else widget
    title t = padRight Max (str t) <=> hBorder

    artistsWidget = columnWidget ArtistsColumn (state^.libraryArtistsL)
    albumsWidget = columnWidget AlbumsColumn (state^.libraryAlbumsL)
    songsWidget = columnWidget SongsColumn (state^.librarySongsL)

    columnWidget col = renderList (listDrawElement col activeColumn) False
    activeColumn = state^.libraryActiveColumnL

listDrawElement :: LibraryColumn -> LibraryColumn -> Bool -> Text -> Widget n
listDrawElement column activeColumn sel el = hCenter $ formatListElement column activeColumn sel $ pad $ str (unpack el)
  where
    pad w = padLeft Max $ padRight Max w

formatListElement :: LibraryColumn -> LibraryColumn -> Bool -> Widget n -> Widget n
formatListElement column activeColumn sel = withAttr attr
  where
    attr
     | column == activeColumn =
       if sel then selActiveColAttrName else activeColAttrName
     | columnIsBefore column activeColumn =
       if sel then listSelAttrName else listAttrName
     | otherwise = listAttrName

-- TODO: find a better way to do this
-- column != activeColumn must be true before calling this function
columnIsBefore :: LibraryColumn -> LibraryColumn -> Bool
columnIsBefore column activeColumn = case activeColumn of
  ArtistsColumn -> False
  SongsColumn -> True
  AlbumsColumn -> case column of
    ArtistsColumn -> True
    AlbumsColumn -> False
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

mkState :: n -> n -> n -> Library -> LibraryState n
mkState artistsName albumsName songsName library = LibraryState
  { _library = library
  , _filteredLibrary = library
  , _libraryArtists = list artistsName (fromList (keys artists)) 1
  , _libraryAlbums = list albumsName firstArtistAlbums 1
  , _librarySongs = list songsName V.empty 1
  , _libraryActiveColumn = ArtistsColumn
  , _libraryMode = ArtistsAlbumsSongsMode
  , _libraryFilter = Nothing
  }
  where
    artists = library^.artistsL
    firstArtistAlbums = V.fromList . Set.toAscList $ snd $ elemAt 0 artists

handleEvent :: Vty.Event -> LibraryState n -> EventM n (LibraryState n)
handleEvent event state = case event of
  (Vty.EvKey (Vty.KChar 'j') []) -> case activeColumn of
    ArtistsColumn -> return $ nextArtist state
    AlbumsColumn -> return $ nextAlbum state
    SongsColumn -> return $ nextSong state
  (Vty.EvKey (Vty.KChar 'k') []) -> case activeColumn of
    ArtistsColumn -> return $ previousArtist state
    AlbumsColumn -> return $ previousAlbum state
    SongsColumn -> return $ previousSong state
  (Vty.EvKey (Vty.KChar 'l') []) -> return $ nextColumn state
  (Vty.EvKey (Vty.KChar 'h') []) -> return $ previousColumn state
  (Vty.EvKey (Vty.KChar 'a') []) -> void (liftIO (addSelectedToPlaylist state)) >> return (next state)
  (Vty.EvKey Vty.KEnter []) -> void (liftIO (addToPlaylistAndPlay state)) >> return (next state)
  -- (Vty.EvKey (Vty.KChar 't') []) -> return $ state & libraryModeL %~ toggleMode
  (Vty.EvKey (Vty.KChar 't') []) -> return $ toggleMode state
  _ -> return state
  where activeColumn = state^.libraryActiveColumnL

next :: LibraryState n -> LibraryState n
next state = case state^.libraryActiveColumnL of
  ArtistsColumn -> nextArtist state
  AlbumsColumn -> nextAlbum state
  SongsColumn -> nextSong state

nextArtist :: LibraryState n -> LibraryState n
nextArtist state = updateArtistAlbumSongs $ updateArtistAlbums (state & libraryArtistsL %~ listMoveDown)

previousArtist :: LibraryState n -> LibraryState n
previousArtist state = updateArtistAlbumSongs $ updateArtistAlbums (state & libraryArtistsL %~ listMoveUp)

nextAlbum :: LibraryState n  -> LibraryState n
nextAlbum state = updateFn $ state & libraryAlbumsL %~ listMoveDown
  where updateFn = case state^.libraryModeL of
          ArtistsAlbumsSongsMode -> updateArtistAlbumSongs
          AlbumsSongsMode -> updateAlbumSongs

previousAlbum :: LibraryState n -> LibraryState n
previousAlbum state = updateFn $ state & libraryAlbumsL %~ listMoveUp
  where updateFn = case state^.libraryModeL of
          ArtistsAlbumsSongsMode -> updateArtistAlbumSongs
          AlbumsSongsMode -> updateAlbumSongs

nextSong :: LibraryState n -> LibraryState n
nextSong state = state & librarySongsL %~ listMoveDown

previousSong :: LibraryState n -> LibraryState n
previousSong state = state & librarySongsL %~ listMoveUp

updateArtistAlbums :: LibraryState n -> LibraryState n
updateArtistAlbums state = state & libraryAlbumsL %~ listReplace newAlbums (Just 0)
  where
    selArtist = snd <$> listSelectedElement (state^.libraryArtistsL)
    newAlbums = case selArtist of
      Just a -> fromMaybe V.empty ((V.fromList . Set.toAscList) <$> lookup a (state^.filteredLibraryL.artistsL))
      Nothing -> V.empty

updateArtistAlbumSongs :: LibraryState n -> LibraryState n
updateArtistAlbumSongs state = state & librarySongsL %~ listReplace (toTxt newSongsFiltered) (Just 0)
  where
    selAlbum = snd <$> listSelectedElement (state^.libraryAlbumsL)
    selArtist = snd <$> listSelectedElement (state^.libraryArtistsL)
    toTxt = map (tag Title "<no title>")
    newSongsFiltered = case selArtist of
      Just a -> filter (\s -> tag Artist "" s == a) newSongs
      Nothing -> newSongs
    newSongs = case selAlbum of
      Just a -> fromMaybe V.empty (lookup a (state^.filteredLibraryL.albumsL))
      Nothing -> V.empty

updateAlbumSongs :: LibraryState n -> LibraryState n
updateAlbumSongs state = state & librarySongsL %~ listReplace (toTxt newSongs) (Just 0)
  where
    selAlbum = snd <$> listSelectedElement (state^.libraryAlbumsL)
    toTxt = map (tag Title "<no title>")
    newSongs = case selAlbum of
      Just a -> fromMaybe V.empty (lookup a (state^.filteredLibraryL.albumsL))
      Nothing -> V.empty

nextColumn :: LibraryState n -> LibraryState n
nextColumn state = state & libraryActiveColumnL .~ nextCol
  where
    activeColumn = state^.libraryActiveColumnL
    nextCol = case state^.libraryModeL of
      ArtistsAlbumsSongsMode -> case activeColumn of
                ArtistsColumn -> AlbumsColumn
                AlbumsColumn -> SongsColumn
                SongsColumn -> SongsColumn
      AlbumsSongsMode -> SongsColumn

previousColumn :: LibraryState n  -> LibraryState n
previousColumn state = state & libraryActiveColumnL .~ prevCol
  where
    activeColumn = state^.libraryActiveColumnL
    prevCol = case state^.libraryModeL of
      ArtistsAlbumsSongsMode -> case activeColumn of
                ArtistsColumn -> ArtistsColumn
                AlbumsColumn -> ArtistsColumn
                SongsColumn -> AlbumsColumn
      AlbumsSongsMode -> AlbumsColumn

getSelected :: LibraryState n -> (Maybe Text, Maybe Text, Maybe Text)
getSelected state = (artist, album, title)
  where
    activeColumn = state^.libraryActiveColumnL
    artist = listGetSelected $ state^.libraryArtistsL
    album = if activeColumn == AlbumsColumn then
      listGetSelected $ state^.libraryAlbumsL else Nothing
    title = if activeColumn == SongsColumn then
      listGetSelected $ state^.librarySongsL else Nothing

addSelectedToPlaylist :: LibraryState n -> IO ()
addSelectedToPlaylist state = addToPlaylist $ getSelected state

addToPlaylistAndPlay :: LibraryState n -> IO ()
addToPlaylistAndPlay state = MPD.addToPlaylistAndPlay $ getSelected state

toggleMode :: LibraryState n -> LibraryState n
toggleMode state = case state^.libraryFilterL of
  Nothing -> newModeState
  Just f -> applyFilter f newModeState
  where
    newModeState = case state^.libraryModeL of
      ArtistsAlbumsSongsMode -> setMode AlbumsSongsMode state
      AlbumsSongsMode -> setMode ArtistsAlbumsSongsMode state
  -- AlbumsSongsMode -> SongsMode
  -- SongsMode -> ArtistsAlbumsSongsMode

setMode :: LibraryMode -> LibraryState n -> LibraryState n
setMode mode state = case mode of
  ArtistsAlbumsSongsMode -> updateArtistAlbumSongs $ updateArtistAlbums $ state & libraryModeL .~ ArtistsAlbumsSongsMode
                                  & libraryActiveColumnL .~ ArtistsColumn
  AlbumsSongsMode -> updateAlbumSongs $ state & libraryModeL .~ AlbumsSongsMode
                                  & libraryActiveColumnL .~ AlbumsColumn
                                  & libraryAlbumsL %~ listReplace (fromList (keys (state^.libraryL.albumsL))) (Just 0)

-- TODO: modify this to filter depending on the mode (artists/albums or albums)
-- Only filter on artists atm
-- filter :: (Text -> Text) -> LibraryState -> LibraryState
applyFilter :: ([Text] -> [Text]) -> LibraryState n -> LibraryState n
applyFilter f state = newState & libraryFilterL .~ Just f
  where
    newState = case state^.libraryModeL of
      ArtistsAlbumsSongsMode -> filterArtists f state
      AlbumsSongsMode -> filterAlbums f state

filterArtists :: ([Text] -> [Text]) -> LibraryState n -> LibraryState n
filterArtists f state = state & libraryArtistsL %~ listReplace (V.fromList filteredArtists) (Just 0)
  where
    filteredArtists = f (keys (state^.libraryL.artistsL))

filterAlbums :: ([Text] -> [Text]) -> LibraryState n -> LibraryState n
filterAlbums f state = state & libraryAlbumsL %~ listReplace (V.fromList filteredAlbums) (Just 0)
  where
    filteredAlbums = f (keys (state^.libraryL.albumsL))

resetFilter :: LibraryState n -> LibraryState n
resetFilter state = newState & libraryFilterL .~ Nothing
  where
    newState = case state^.libraryModeL of
      ArtistsAlbumsSongsMode -> updateArtistAlbumSongs $ updateArtistAlbums $ state & libraryArtistsL %~ listReplace artists (Just 0)
        where artists = V.fromList . keys $ state^.libraryL.artistsL
      AlbumsSongsMode -> updateAlbumSongs $ state & libraryAlbumsL %~ listReplace albums (Just 0)
        where albums = V.fromList . keys $ state^.libraryL.albumsL
