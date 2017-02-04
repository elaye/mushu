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
import Data.Vector ((!), empty)

import qualified UI.Views.Main as Main
import Network.MPD (Song(..), Metadata(..), toString)

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

import Library (Library(..), ArtistName, AlbumName, albumsL, artistsL)

import UI.Types (AppState(..), UIName(..), ActiveColumn(..), AppException(..), library, libraryActiveColumn, filteredLibrary, libraryArtists, libraryAlbums, librarySongs, filterEditor, filterActive, filterFocused)

import Text.Fuzzy (simpleFilter)
import Brick.Widgets.Edit (Editor, applyEdit, renderEditor, handleEditorEvent, getEditContents)
import Data.Text.Zipper (textZipper)

type NextState = EventM UIName (Next AppState)

draw :: AppState -> [Widget UIName]
draw state = Main.draw state $ widget
  where
    fzf = str "Filter: " <+> renderEditor True (state^.filterEditor) <+> (padLeft Max (str "Enter: apply | Esc: disable "))
    columns = column "Artists" True artistsWidget <+> column "Albums" True albumsWidget <+> column "Songs" False songsWidget
    column name bBorder widget = (title name) <=> if bBorder then (widget <+> vBorder) else widget
    title t = (padRight Max $ str t) <=> hBorder

    artistsWidget = columnWidget ArtistsColumn (state^.libraryArtists)
    albumsWidget = columnWidget AlbumsColumn (state^.libraryAlbums)
    songsWidget = columnWidget SongsColumn (state^.librarySongs)

    columnWidget column els = renderList (listDrawElement (activeColumn == column)) True els
    activeColumn = state^.libraryActiveColumn
    widget = case (state^.filterActive) of
      True -> fzf <=> hBorder <=> columns
      False -> columns

listDrawElement :: Bool -> Bool -> Text -> Widget UIName
listDrawElement colActive sel el = hCenter $ formatListElement colActive sel $ pad $ str (unpack el)
  where
    pad w = padLeft Max $ padRight Max $ w

formatListElement :: Bool -> Bool -> Widget UIName -> Widget UIName
formatListElement colActive sel widget = withAttr attr widget
  where attr = case colActive of
                True -> case sel of
                  True -> selActiveColAttrName
                  False -> activeColAttrName
                False -> case sel of
                  True -> selAttrName
                  False -> listAttr

selAttrName :: AttrName
selAttrName = listSelectedAttr <> "custom"

activeColAttrName :: AttrName
activeColAttrName = listAttr <> "active-column"

selActiveColAttrName :: AttrName
selActiveColAttrName = listSelectedAttr <> "selected-active-column"

attrs :: [(AttrName, Vty.Attr)]
attrs = [ (selAttrName, Vty.brightBlack `on` Vty.black)
        , (selActiveColAttrName, Vty.green `on` Vty.black)
        , (listAttr, fg Vty.brightBlack)
        , (activeColAttrName, fg Vty.white)
        ]

event :: AppState -> BrickEvent UIName e -> EventM UIName (Next AppState)
event state (VtyEvent e) = case (state^.filterFocused) of
  True -> case e of
    (Vty.EvKey Vty.KEsc []) -> continue $ resetFilter state
    (Vty.EvKey Vty.KEnter []) -> continue (state & filterFocused .~ False)
    ev -> do
      newFilterEditor <- handleEditorEvent ev (state^.filterEditor)
      let newEditorState = state & filterEditor .~ newFilterEditor
      continue $ newEditorState & libraryArtists .~ (list (UIName "filtered-artists") (V.fromList (getFilteredLibrary newEditorState)) 1)
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
    -- (Vty.EvKey Vty.KEnter []) -> play state
    (Vty.EvKey (Vty.KChar 'q') []) -> halt state
    (Vty.EvKey (Vty.KChar 'f') []) -> continue (state & filterActive .~ True
                                                  & filterFocused .~ True)
    (Vty.EvKey Vty.KEsc []) -> case (state^.filterActive) of
      True -> continue $ resetFilter state
      False -> continue state
    -- VtyEvent (Vty.EvKey (Vty.KChar '1') []) -> changeView state 1
    {-Vty.EvKey (Vty.KChar '-') [] -> delete-}
    -- ev -> listEvent ev state
    (Vty.EvKey (Vty.KChar 'e') []) -> liftIO $ throw MPDException
    ev -> continue state
  where activeColumn = state^.libraryActiveColumn
event state _ = continue state

-- getFilteredLibrary :: AppState -> List UIName ArtistName
-- getFilteredLibrary state = filterLibrary ft (state^.libraryArtists)
--   where ft = concat $ getEditContents (state^.filterEditor)

resetFilter :: AppState -> AppState
resetFilter state = state & filterActive .~ False
                          & filterFocused .~ False
                          & filterEditor %~ clearFilterEditor
                          & libraryArtists .~ artistsWidget
  where
    artistsWidget = list (UIName "filtered-artists") artists 1
    artists = V.fromList . keys $ state^.library.artistsL

getFilteredLibrary :: AppState -> [ArtistName]
getFilteredLibrary state = filterLibrary ft (keys (state^.library.artistsL))
  where ft = concat $ getEditContents (state^.filterEditor)

filterLibrary :: Text -> [ArtistName] -> [ArtistName]
filterLibrary ft as = pack <$> filtered
  where filtered = simpleFilter (unpack ft) (unpack <$> as)

clearFilterEditor :: Editor Text UIName -> Editor Text UIName
clearFilterEditor editor = applyEdit (\_ -> textZipper [""] (Just 1)) editor

nextArtist :: AppState -> NextState
nextArtist state = continue $ updateSongs $ updateAlbums (state & libraryArtists %~ listMoveDown)

previousArtist :: AppState -> NextState
previousArtist state = continue $ updateSongs $ updateAlbums (state & libraryArtists %~ listMoveUp)

updateAlbums :: AppState -> AppState
updateAlbums state = state & libraryAlbums .~ (list (UIName "albums") newAlbums 1)
  where
    selArtist = snd <$> (listSelectedElement $ state^.libraryArtists)
    newAlbums = case selArtist of
      -- Just a -> maybe [] (\as -> (keys (as^.albums))) (lookup a (state^.filteredLibrary.artistAlbums))
      Just a -> fromMaybe V.empty ((V.fromList . Set.toAscList) <$> (lookup a (state^.filteredLibrary.artistsL)))
      Nothing -> V.empty

updateSongs :: AppState -> AppState
updateSongs state = state & librarySongs .~ (map (tag Title "<no title>") newSongsWidget)
  where
    tag key def song = concat (pack <$> toString <$> findWithDefault [fromString def] key (sgTags song))
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

nextAlbum :: AppState -> NextState
nextAlbum state = continue $ state & libraryAlbums %~ listMoveDown

previousAlbum :: AppState -> NextState
previousAlbum state = continue $ state & libraryAlbums %~ listMoveUp

nextSong :: AppState -> NextState
nextSong state = continue $ state & librarySongs %~ listMoveDown

previousSong :: AppState -> NextState
previousSong state = continue $ state & librarySongs %~ listMoveUp

nextColumn :: AppState -> NextState
nextColumn state = continue $ state & libraryActiveColumn .~ nextCol
  where
    nextCol = case (state^.libraryActiveColumn) of
                ArtistsColumn -> AlbumsColumn
                AlbumsColumn -> SongsColumn
                SongsColumn -> SongsColumn

previousColumn :: AppState -> NextState
previousColumn state = continue $ state & libraryActiveColumn .~ prevCol
  where
    prevCol = case (state^.libraryActiveColumn) of
                ArtistsColumn -> ArtistsColumn
                AlbumsColumn -> ArtistsColumn
                SongsColumn -> AlbumsColumn

-- libraryEvent :: Vty.Event -> AppState -> NextState
-- libraryEvent event state = continue =<< (\m -> state & filteredLibrary .~ m) <$> handleLibraryEvent event (state^.library)
