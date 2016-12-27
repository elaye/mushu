{-# LANGUAGE OverloadedStrings #-}
module Main where

import ClassyPrelude

import MPD
import UI (start)

main :: IO ()
main = do
  currentSong
  start
