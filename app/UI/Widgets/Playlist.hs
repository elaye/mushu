{-# LANGUAGE OverloadedStrings #-}

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
import Brick.Widgets.List (List, renderList, listSelectedAttr, listAttr)
import Brick.AttrMap (AttrName)
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.Core ((<+>), str, withAttr, padLeft, padRight, hLimit)
import Brick.Util (fg, on)

import qualified Graphics.Vty as V

import UI.Types (AppState(..), UIName)
import UI.Utils (secondsToTime)

-- import Data.Map.Lazy (findWithDefault)
import Network.MPD (Song(..), Metadata(..), toString, Value)

mkWidget :: List UIName Song -> Widget UIName
mkWidget playlist = renderList listDrawElement True playlist

listDrawElement :: Bool -> Song -> Widget UIName
listDrawElement sel song = hCenter $ formatListElement sel False $ artist <+> track <+> title <+> album <+> time
  where
    -- pad w = padLeft Max $ padRight Max $ w
    -- frm = padRight (Pad 1) $ hLimit 15 $ str $ (formatFrom mail) ++ "          "
    -- sbj = pad $ str $ formatSubject mail
    -- dt = padRight (Pad 1) $ str $ formatDate mail
    pad w = padLeft Max $ padRight Max $ w
    -- artist = str $ findWithDefault "Kendrick Lamar" Artist
    -- artist = str $ toString $ findWithDefault Artist (fromString "Kendrick Lamar") (sgTags song)
    -- artist = str $ toString $ findWithDefault ["Kendrick Lamar"] Artist (sgTags song)
    tag key def = concat (toString <$> findWithDefault [fromString def] key (sgTags song))

    artist = padRight (Pad 1) $ hLimit 15 $ str $ tag Artist "<no artist>"
    track = padRight (Pad 2) $ str $ tag Track "<?>"
    title = str $ tag Title "<no title>"
    album = str $ tag Album "<no album>"
    time = padLeft Max $ str $ unpack $ secondsToTime $ sgLength song

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

attrs :: [(AttrName, V.Attr)]
attrs = [ (selAttrName, fg V.black)
        , (newAttrName, V.green `on` V.black)
        , (playingAttrName, fg V.brightBlack)
        , (selPlayingAttrName, V.brightBlack `on` V.black)
        ]
