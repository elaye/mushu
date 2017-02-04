module UI.Widgets.Playlist
( mkWidget
, attrs
) where

import ClassyPrelude hiding ((<>), on)
import qualified Prelude as UnsafePrelude
import Data.Monoid ((<>))
import Data.Text (replace)

import Data.Time.Format (defaultTimeLocale, TimeLocale(..))
import Data.Time.LocalTime (LocalTime(..))

import Lens.Micro.Platform ((^.), _2, _3)

import Brick.Types (Widget(..), Padding(..))
import Brick.Widgets.List (List, renderList, listSelectedAttr, listAttr, listElementsL)
import Brick.AttrMap (AttrName)
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.Core ((<+>), str, withAttr, padLeft, padRight, hLimit)
import Brick.Util (fg, on)

import qualified Graphics.Vty as V

import UI.Types (AppState(..), UIName)
import UI.Utils (secondsToTime)

-- import Data.Map.Lazy (findWithDefault)
import Network.MPD (Song(..), Metadata(..), toString, Value)
import MPD (tag)

mkWidget :: List UIName Song -> Widget UIName
mkWidget playlist = renderList listDrawElement True playlist

listDrawElement ::  Bool -> Song -> Widget UIName
listDrawElement sel song = hCenter $ formatListElement False sel $ artist <+> track <+> title <+> album <+> time
  where
    artist = column (Just 25) (Pad 0) Max $ str. unpack $ tag Artist "<unknown>" song
    track = column (Just 4) Max (Pad 0) $ str. unpack $ tag Track "?" song
    title = column Nothing (Pad 2) Max $ str . unpack $ tag Title "<no title>" song
    album = column (Just 35) (Pad 2) Max $ str . unpack $ tag Album "<no album>" song
    time = column (Just 8) Max (Pad 1) $ str . unpack $ secondsToTime $ sgLength song

-- Make a column considering a left padding, a right padding and an optional width
column :: Maybe Int -> Padding -> Padding -> Widget UIName -> Widget UIName
column maybeWidth left right widget = case maybeWidth of
  Nothing -> w
  Just wth -> hLimit wth $ w
  where w = padLeft left $ padRight right $ widget

formatListElement :: Bool -> Bool -> Widget UIName -> Widget UIName
formatListElement playing sel widget = withAttr attr widget
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

attrs :: [(AttrName, V.Attr)]
attrs = [ (playlistListAttrName, fg V.white)
        , (playlistPlayingAttrName, V.withStyle (fg V.white) V.bold)
        , (playlistSelAttrName, V.withStyle (V.green `on` V.black) V.standout)
        , (playlistSelPlayingAttrName, V.withStyle (V.withStyle (V.green `on` V.black) V.standout) V.bold)
        ]
