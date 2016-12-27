{-# LANGUAGE OverloadedStrings #-}

module UI.Utils
( attrs
, secondsToTime
) where

import ClassyPrelude hiding (on)

import Brick.Widgets.List (listAttr, listSelectedAttr)
import Brick.Util (fg, on)
import Brick.AttrMap (AttrName)

import qualified Graphics.Vty as V

attrList :: (AttrName, V.Attr)
attrList = (listAttr, fg V.white)

attrSelectedList :: (AttrName, V.Attr)
attrSelectedList = (listSelectedAttr, V.black `on` V.white)

attrs :: [(AttrName, V.Attr)]
attrs = [ attrList
        , attrSelectedList
        ]

secondsToTime :: Integer -> Text
secondsToTime secs = txt
  where
    txt = case (h == 0) of
      True -> (tshow m) ++ ":" ++ sTxt
      False -> (tshow h) ++ (pad (tshow m) "0") ++ ":" ++ sTxt
    sTxt = pad (tshow s) "0"
    s = secs `mod` 60
    m = ((secs - s) `div` 60) `mod` 60
    h = ((secs - s) `div` (60 * 60)) `mod` 60

pad :: Text -> Text -> Text
pad txt p = case (length txt == 1) of
  True -> p ++ txt
  False -> txt
