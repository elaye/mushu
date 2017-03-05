{-# LANGUAGE TemplateHaskell #-}
module UI.Widgets.Playlist
( PlaylistState
, mkState
, mkWidget
, handleEvent
, update
, playingSongL
, attrs
) where

import ClassyPrelude hiding ((<>), on)
import Data.Monoid ((<>))
import TH (makeSuffixLenses)
import Lens.Micro.Platform ((^.), (%~), (&))

import Brick.Types (Widget(..), Padding(..), EventM)
import Brick.Widgets.List (List, list, renderList, listSelectedAttr, listAttr, listMoveDown, listMoveUp, listSelectedL, listElementsL, listReplace)
import Brick.AttrMap (AttrName)
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.Core ((<=>), (<+>), str, withAttr, padLeft, padRight, hLimit)
import Brick.Widgets.Border (hBorder)
import Brick.Util (fg, on)

import Data.Vector ((!))

import qualified Graphics.Vty as Vty

import UI.Utils (secondsToTime)

import Network.MPD (Song(..), Metadata(..), withMPD, playId)
-- import Network.MPD (withMPD, Song(..), Id(..), playId)
import MPD (tag)

data PlaylistState n = PlaylistState
  { _songs :: List n Song
  , _playingSong :: Maybe Song
  }

mkState :: n -> [Song] -> PlaylistState n
mkState name songs = PlaylistState
  { _songs = list name (fromList songs) 1
  , _playingSong = Nothing
  }

makeSuffixLenses ''PlaylistState

-- mkWidget :: (Show n, Ord n) => List n Song -> Widget n
mkWidget :: (Show n, Ord n) => PlaylistState n -> Widget n
mkWidget state = header <=> hBorder <=> renderList listDrawElement True (state^.songsL)
  where
    header = artist <+> track <+> title <+> album <+> time
    artist = column (Just 25) (Pad 0) Max $ str "Artist"
    track = column (Just 5) Max (Pad 0) $ str "Track"
    title = column Nothing (Pad 2) Max $ str "Title"
    album = column (Just 35) (Pad 2) Max $ str "Album"
    time = column (Just 8) Max (Pad 1) $ str "Time"

update :: [Song] -> PlaylistState n -> PlaylistState n
update songs state = state & songsL %~ (listReplace (fromList songs) (Just 0))

handleEvent :: Vty.Event -> PlaylistState n -> EventM n (PlaylistState n)
handleEvent event state = case event of
  (Vty.EvKey (Vty.KChar 'j') []) -> return $ nextSong state
  (Vty.EvKey (Vty.KChar 'k') []) -> return $ previousSong state
  (Vty.EvKey Vty.KEnter []) -> play state
  _ -> return state

nextSong :: PlaylistState n -> PlaylistState n
nextSong state = state & songsL %~ listMoveDown

previousSong :: PlaylistState n -> PlaylistState n
previousSong state = state & songsL %~ listMoveUp

play :: PlaylistState n -> EventM n (PlaylistState n)
play state = case (state^.songsL.listSelectedL) of
  Nothing -> return state
  Just i -> do
    let selectedSong = (state^.songsL.listElementsL) ! i
    case (sgId selectedSong) of
      Nothing -> return state
      Just id -> do
        _ <- liftIO $ withMPD $ playId id
        return state

listDrawElement ::  Bool -> Song -> Widget n
listDrawElement sel song = hCenter $ formatListElement False sel $ artist <+> track <+> title <+> album <+> time
  where
    artist = column (Just 25) (Pad 0) Max $ str. unpack $ tag Artist "<unknown>" song
    track = column (Just 5) Max (Pad 0) $ str. unpack $ tag Track "?" song
    title = column Nothing (Pad 2) Max $ str . unpack $ tag Title "<no title>" song
    album = column (Just 35) (Pad 2) Max $ str . unpack $ tag Album "<no album>" song
    time = column (Just 8) Max (Pad 1) $ str . unpack $ secondsToTime $ sgLength song

-- Make a column considering a left padding, a right padding and an optional width
column :: Maybe Int -> Padding -> Padding -> Widget n -> Widget n
column maybeWidth left right widget = case maybeWidth of
  Nothing -> w
  Just wth -> hLimit wth w
  where w = padLeft left $ padRight right widget

formatListElement :: Bool -> Bool -> Widget n -> Widget n
formatListElement playing sel = withAttr attr
  where attr = case playing of
                True -> case sel of
                  True -> playlistSelPlayingAttrName
                  False -> playlistPlayingAttrName
                False -> case sel of
                  True -> playlistSelAttrName
                  False -> playlistListAttrName

playlistListAttrName :: AttrName
playlistListAttrName = listAttr <> "playlist"

playlistSelAttrName :: AttrName
playlistSelAttrName = listSelectedAttr <> "playlist-selected"

playlistPlayingAttrName :: AttrName
playlistPlayingAttrName = listAttr <> "playlist-playing"

playlistSelPlayingAttrName :: AttrName
playlistSelPlayingAttrName = listSelectedAttr <> "playlist-selected-playing"

attrs :: [(AttrName, Vty.Attr)]
attrs = [ (playlistListAttrName, fg Vty.white)
        , (playlistPlayingAttrName, Vty.withStyle (fg Vty.white) Vty.bold)
        , (playlistSelAttrName, Vty.withStyle (Vty.green `on` Vty.black) Vty.standout)
        , (playlistSelPlayingAttrName, Vty.withStyle (Vty.withStyle (Vty.green `on` Vty.black) Vty.standout) Vty.bold)
        ]
