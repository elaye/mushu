module Main where

import ClassyPrelude

import Network.MPD
import UI (start)

main :: IO ()
main = do
  -- res <- withMPD $ listAllInfo (fromString "")
  -- print res
  print $ (maxBound :: Int)
  start
