{-# LANGUAGE OverloadedStrings #-}
module Main where

import ClassyPrelude

import Network.MPD
import UI (start)

main :: IO ()
main = do
  start
