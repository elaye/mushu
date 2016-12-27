{-# LANGUAGE OverloadedStrings #-}
module UI.Widgets.Mail
( mkWidget
) where

import ClassyPrelude

import Brick.Types (Widget)
import Brick.Widgets.Core (str)

import Lens.Micro.Platform ((^.))

import Mail (Mail, body)
import UI.Types (UIName)

mkWidget :: Maybe Mail -> Widget UIName
mkWidget Nothing = str ""
mkWidget (Just mail) = str $ unpack $ concat $ mail^.body
