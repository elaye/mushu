module UI.Widgets.Status
( attrs
, attrName
, mkWidget
) where

import ClassyPrelude hiding (on)

import UI.Types

import Lens.Micro.Platform ((^.))
import Brick.Types (Widget, Padding(..))
import Brick.Widgets.Core (str, withAttr, padRight, padLeft, hBox, hLimit, (<=>))
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.List (listElementsL)
import Brick.AttrMap (AttrName)
import Brick.Util (on)

import qualified Graphics.Vty as V
import Data.Vector ((!?))


import Network.MPD (Metadata(..), Status(..), Song)
import MPD (tag)

attrName :: AttrName
attrName = "status"

attrs :: [(AttrName, V.Attr)]
attrs = [(attrName, V.green `on` V.black)]

mkWidget :: AppState -> Widget UIName
mkWidget state = hCenter $ withAttr attrName $ widgets
  where
    -- widgets = vBox $
    --   [ title <+> volume
    --   , (hLimit 15 (padRight Max playbackState)) <+> artist <+> (hLimit 15 (padLeft Max (str " ")))
    --   ]
    widgets = hBox $
      [ str " " <=> (hLimit 15 (padRight Max playbackState))
      , title <=> artist
      , str " " <=> (hLimit 15 (padLeft Max volume))
      ]
    st = state^.status
    currentSong = (stSongPos st) >>= (\i -> (state^.playlist.listElementsL) !? i)
    title = hCenter $ mkTagWidget Title "<untitled>" currentSong
    artist = hCenter $ mkTagWidget Artist "<unknown artist>" currentSong
    playbackState = mkStateWidget st
    volume = mkVolumeWidget st

mkStateWidget :: Status -> Widget UIName
mkStateWidget status = str $ "[" ++ state ++ "]"
  where state = toLower <$> show $ (stState status)

mkVolumeWidget :: Status -> Widget UIName
mkVolumeWidget status = str $ "Vol: " ++ (fromMaybe "-" $ maybeVolume)
  where maybeVolume = (\v -> show v ++ "%") <$> (stVolume status)

mkTagWidget :: Metadata -> Text -> Maybe Song -> Widget UIName
mkTagWidget metaTag def song = str . unpack $ fromMaybe " " $ (tag metaTag def) <$> song
