module UI.Widgets.Status
( attrs
, attrName
, mkWidget
) where

import ClassyPrelude hiding (on)

import UI.Types

import Lens.Micro.Platform ((^.))
import Brick.Types (Widget, Padding(..))
import Brick.Widgets.Core (str, withAttr, padRight, vBox, (<+>))
import Brick.Widgets.Center (hCenter)
import Brick.AttrMap (AttrName)
import Brick.Util (on)

import qualified Graphics.Vty as V


import Network.MPD (Status(..))

attrName :: AttrName
attrName = "status"

attrs :: [(AttrName, V.Attr)]
attrs = [(attrName, V.green `on` V.black)]

mkWidget :: AppState -> Widget UIName
mkWidget state = hCenter $ withAttr attrName $ widgets
  where
    widgets = vBox $
      [ currentSong <+> volume
      , padRight Max $ str $ show elapsed
      ]
    st = state^.status
    elapsed = case stTime st of
      Nothing -> 0
      Just (e, t) -> e
    currentSong = str $ show $ stSongID st
    volume = str $ "Vol: " ++ (fromMaybe "-" $ (\v -> show v ++ "%") <$> (stVolume st))
