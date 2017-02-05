module UI.Widgets.Status
( attrs
, attrName
, mkWidget
) where

import ClassyPrelude hiding (on, (<>))
import Data.Monoid ((<>))

import UI.Types

import Lens.Micro.Platform ((^.))
import Brick.Types (Widget, Padding(..))
import Brick.Widgets.Core (str, withAttr, padRight, padLeft, hBox, hLimit, (<=>), (<+>))
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.List (listElementsL)
import Brick.AttrMap (AttrName)
import Brick.Util (on, fg)

import qualified Graphics.Vty as V
import Data.Vector ((!?))


import Network.MPD (Metadata(..), Status(..), Song, State(..))
import MPD (tag)

attrName :: AttrName
attrName = "status"

artistAttrName :: AttrName
artistAttrName = attrName <> "artist"

albumAttrName :: AttrName
albumAttrName = attrName <> "album"

titleAttrName :: AttrName
titleAttrName = attrName <> "title"

playingAttrName :: AttrName
playingAttrName = attrName <> "playing"

pausedAttrName :: AttrName
pausedAttrName = attrName <> "paused"

stoppedAttrName :: AttrName
stoppedAttrName = attrName <> "stopped"

attrs :: [(AttrName, V.Attr)]
attrs = [ (attrName, fg V.white)
        , (artistAttrName, fg V.white)
        , (titleAttrName, fg V.yellow)
        , (albumAttrName, fg V.blue)
        , (playingAttrName, fg V.green)
        , (pausedAttrName, fg V.yellow)
        , (stoppedAttrName, fg V.red)
        ]

mkWidget :: AppState -> Widget UIName
mkWidget state = hCenter $ withAttr attrName $ widgets
  where
    widgets = hBox $
      [ str " " <=> (hLimit 15 (padRight Max playbackState))
      , title <=> artistAlbum
      , str " " <=> (hLimit 15 (padLeft Max volume))
      ]
    st = state^.status
    currentSong = (stSongPos st) >>= (\i -> (state^.playlist.listElementsL) !? i)
    title = hCenter $ withAttr titleAttrName $ mkTagWidget Title "<untitled>" currentSong
    artist = withAttr artistAttrName $ mkTagWidget Artist "<unknown artist>" currentSong
    album = withAttr albumAttrName $ mkTagWidget Album "<unknown album>" currentSong
    artistAlbum = hCenter $ artist <+> (str " - ") <+> album
    playbackState = mkStateWidget st
    volume = mkVolumeWidget st

mkStateWidget :: Status -> Widget UIName
mkStateWidget status = withAttr attr $ str $ "[" ++ state ++ "]"
  where
    st = stState status
    -- state = toLower <$> (show st)
    state = (show st)
    attr = case st of
      Playing -> playingAttrName
      Paused -> pausedAttrName
      Stopped -> stoppedAttrName

mkVolumeWidget :: Status -> Widget UIName
mkVolumeWidget status = str $ "Vol: " ++ (fromMaybe "-" $ maybeVolume)
  where maybeVolume = (\v -> show v ++ "%") <$> (stVolume status)

mkTagWidget :: Metadata -> Text -> Maybe Song -> Widget UIName
mkTagWidget metaTag def song = str . unpack $ fromMaybe " " $ (tag metaTag def) <$> song
