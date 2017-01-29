module UI.Widgets.Status
( attrs
, attrName
, mkWidget
) where

import ClassyPrelude hiding (on)

import Brick.Types (Widget, Padding(..))
import Brick.Widgets.Core (str, withAttr, padRight)
import Brick.Widgets.Center (hCenter)
import Brick.AttrMap (AttrName)
import Brick.Util (on)

import qualified Graphics.Vty as V

import Config (Account(..))
import UI.Types (UIName)

attrName :: AttrName
attrName = "status"

attrs :: [(AttrName, V.Attr)]
attrs = [(attrName, V.green `on` V.black)]

mkWidget :: Account -> Widget UIName
mkWidget account = hCenter $ withAttr attrName $ padRight Max $ str $ "Status | Current account: " ++ (unpack $ name account)
