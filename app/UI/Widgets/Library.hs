{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module UI.Widgets.Library
( fetchLibrary
, libraryMoveUp
, libraryMoveDown
, Library(..)
, ArtistAlbums(..)
, artistAlbums
, albums
, mkWidget
, attrs
) where

import ClassyPrelude hiding ((<>), on)
import Data.Monoid ((<>))

import Lens.Micro.Platform (makeLenses, (^.), (&), (.~))

import qualified Network.MPD as M
import Network.MPD ((=?), (<&>))

import qualified Brick.Widgets.List as L
import Brick.Widgets.Core (Named(..), withDefAttr, visible, viewport, translateBy, vBox, withAttr, str, padRight)
import Brick.Types (Widget(..), EventM, Size(..), getContext, availHeightL, ViewportType(..), Location(..), Padding(..))
import Brick.AttrMap (AttrName)
import Brick.Util (clamp, fg, on)
import Graphics.Vty (Event(..), Key(..), white, black, green, brightBlack, Attr(..))

import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Data.Vector ((!))

type ArtistName = Text
type AlbumName = Text

data ArtistAlbums n = ArtistAlbums
  -- { _albums :: Map AlbumName (Vector M.Song)
  { _albums :: Map AlbumName (L.List n M.Song)
  , _albumKeys :: V.Vector Text
  , _selectedAlbum :: Maybe Int
  } deriving (Show)

makeLenses ''ArtistAlbums

data Library n = Library
  { _artistAlbums :: Map ArtistName (ArtistAlbums n)
  , _artistKeys :: V.Vector Text
  , _selectedArtist :: Maybe Int
  , _name :: n
  } deriving (Show)

makeLenses ''Library

instance Named (Library n) n where
  getName = _name

libraryItemHeight :: Int
libraryItemHeight = 1

-- | The top-level attribute used for the entire library.
libraryAttr :: AttrName
libraryAttr = "library"

-- | The attribute used only for the currently-selected library item when
-- the column or the library does not have focus. Extends 'libraryAttr'.
librarySelectedAttr :: AttrName
librarySelectedAttr = libraryAttr <> "selected"

-- | The attribute used only for the currently-selected library item when
-- the column or the library has focus. Extends 'librarySelectedAttr'.
librarySelectedFocusedAttr :: AttrName
librarySelectedFocusedAttr = librarySelectedAttr <> "focused"

attrs :: [(AttrName, Attr)]
attrs = [ (libraryAttr, fg white)
        , (librarySelectedAttr, black `on` white)
        , (librarySelectedFocusedAttr, fg brightBlack)
        -- , (selPlayingAttrName, V.brightBlack `on` V.black)
        ]

mkWidget :: (Ord n, Show n) => Library n -> Widget n
mkWidget library = renderLibrary True library

-- | Turn a library state value into a widget given an item drawing
-- function.
renderLibrary :: (Ord n, Show n)
           => Bool
           -- ^ Whether the library has focus
           -> Library n
           -- ^ The library to be rendered
           -> Widget n
           -- ^ rendered widget
renderLibrary foc lib =
    withDefAttr libraryAttr $
    drawLibraryElements foc lib

drawLibraryElements :: (Ord n, Show n)
  => Bool
  -- ^ Whether the library has focus
  -> Library n
  -- ^ The map to be rendered
  -> Widget n
drawLibraryElements foc lib = drawArtistElements foc lib artistDrawElement-- <> drawAlbumElements <> drawSongElements

artistDrawElement :: Bool -> ArtistName -> Widget n
artistDrawElement sel artist = padRight Max $ withAttr attr $ str (unpack artist)
  where attr = case sel of
                  True -> librarySelectedAttr
                  False -> libraryAttr

drawArtistElements ::(Ord n, Show n)
  => Bool
  -- ^ Whether the artists column has focus
  -> Library n
  -- ^ The artists to be rendered
  -> (Bool -> ArtistName -> Widget n)
  -- ^ Rendering function, True for the selected element
  -> Widget n
drawArtistElements foc l drawElem = Widget Greedy Greedy $ do
  c <- getContext
  let es = V.slice start num (l^.artistKeys)
      idx = fromMaybe 0 (l^.selectedArtist)

      start = max 0 $ idx - numPerHeight + 1
      num = min (numPerHeight * 2) (V.length (l^.artistKeys) - start)

      numPerHeight = c^.availHeightL

      drawnElements = flip V.imap es $ \i e ->
          let isSelected = Just (i + start) == l^.selectedArtist
              elemWidget = drawElem isSelected e
              selItemAttr = if foc
                            then withDefAttr librarySelectedFocusedAttr
                            else withDefAttr librarySelectedAttr
              makeVisible = if isSelected
                            then visible . selItemAttr
                            else id
          in makeVisible elemWidget

  render $ viewport (l^.name) Vertical $
            translateBy (Location (0, start)) $
            vBox $ V.toList drawnElements

-- drawAlbumElements ::(Ord n, Show n)
--   => Bool
--   -- ^ Whether the artists column has focus
--   -> ArtistAlbums n
--   -- ^ The artists to be rendered
--   -> (Bool -> ArtistName -> Widget n)
--   -- ^ Rendering function, True for the selected element
--   -> Widget n
-- drawAlbumElements foc l drawElem = Widget Greedy Greedy $ do
--   c <- getContext
--   let es = V.slice start num (l^.artistKeys)
--       idx = fromMaybe 0 (l^.selectedArtist)

--       start = max 0 $ idx - numPerHeight + 1
--       num = min (numPerHeight * 2) (V.length (l^.artistKeys) - start)

--       numPerHeight = c^.availHeightL

--       drawnElements = flip V.imap es $ \i e ->
--           let isSelected = Just (i + start) == l^.selectedArtist
--               elemWidget = drawElem isSelected e
--               selItemAttr = if foc
--                             then withDefAttr librarySelectedFocusedAttr
--                             else withDefAttr librarySelectedAttr
--               makeVisible = if isSelected
--                             then visible . selItemAttr
--                             else id
--           in makeVisible elemWidget

--   render $ viewport (l^.name) Vertical $
--             translateBy (Location (0, start)) $
--             vBox $ V.toList drawnElements

-- | Move the map selected index up by one. (Moves the cursor up,
-- subtracts one from the index.)
libraryMoveUp :: Library n -> Library n
libraryMoveUp = libraryMoveBy (-1)

-- | Move the map selected index down by one. (Moves the cursor down,
-- adds one to the index.)
libraryMoveDown :: Library n -> Library n
libraryMoveDown = libraryMoveBy 1

-- | Move the map selected index by the specified amount, subject to
-- validation.
libraryMoveBy :: Int -> Library n -> Library n
libraryMoveBy amt l =
    let newSel = clamp 0 (V.length (l^.artistKeys) - 1) <$> (amt +) <$> (l^.selectedArtist)
    in l & selectedArtist .~ newSel

-- mpdReq :: M.MPD a -> IO [M.Value]
mpdReq :: M.MPD [a] -> IO [a]
mpdReq req = do
  res <- M.withMPD req
  case res of
    Left err -> (print ("MPD error: " ++ (show err))) >> return []
    Right r -> return r

getArtists :: IO [M.Value]
-- getArtists = (take 5) <$> (mpdReq $ M.list M.Artist Nothing)
getArtists = mpdReq $ M.list M.Artist Nothing

getArtistAlbums :: M.Artist -> IO [M.Value]
-- getArtistAlbums artist = (take 5) <$> (mpdReq $ M.list M.Album (Just artist))
getArtistAlbums artist = mpdReq $ M.list M.Album (Just artist)

getArtistAlbumSongs :: M.Artist -> M.Album -> IO [M.Song]
-- getArtistAlbumSongs artist album = (take 3) <$> (mpdReq req)
getArtistAlbumSongs artist album = mpdReq req
  where req = M.search (M.Artist =? artist <&> M.Album =? album)

fetchLibrary ::(Show n, Eq n, IsString n) => n -> IO (Library n)
-- fetchLibrary :: n -> IO (Library n)
fetchLibrary name = do
  artists <- getArtists
  -- print artists
  let getAlbums a = fromList <$> (getArtistAlbums a)
  let getSongs ar al = fromList <$> (getArtistAlbumSongs ar al)
  let kvAlbum ar al = do
        ss <- getSongs ar al
        return (pack (M.toString al), (L.list "list-album-songs" ss 1))
  let kvArtist ar = do
        als <- getAlbums ar
        -- print als
        -- TODO: check that we really have an ordered list of unique keys
        -- let as = Map.fromDistinctAscList $ mapM (kvAlbum ar) als
        as <- Map.fromDistinctAscList <$> (sequence (map (kvAlbum ar) als))
        return (pack (M.toString ar), ArtistAlbums as (V.fromList (Map.keys as)) (Just 0))
  -- TODO: check that we really have an ordered list of unique keys
  -- return $ Map.fromDistinctAscList $ mapM kvArtist artists
  artistsAlbums <- Map.fromDistinctAscList <$> (sequence (map kvArtist artists))
  return $ Library artistsAlbums (V.fromList (Map.keys artistsAlbums)) (Just 0) name

