{-# LANGUAGE TemplateHaskell #-}
module UI.Widgets.Notification
( NotificationState
, mkState
, mkWidget
, setNotification
) where

import ClassyPrelude
import TH (makeSuffixLenses)

import Lens.Micro.Platform ((^.), (&), (.~))

import Brick.Types (Widget)
import Brick.Widgets.Core (str)

data NotificationState = NotificationState
  { _notification :: Maybe Text
  } deriving Show

makeSuffixLenses ''NotificationState

mkState :: NotificationState
mkState = NotificationState Nothing

mkWidget :: NotificationState -> Widget n
mkWidget state = str . unpack $ fromMaybe "" (state^.notificationL)

setNotification :: Text -> NotificationState -> NotificationState
setNotification txt state = state & notificationL .~ (Just txt)
