module MPD
( currentSong
) where

import ClassyPrelude

import Network.MPD
  ( withMPD
  )
import qualified Network.MPD as M

currentSong :: IO ()
currentSong = do
  res <- withMPD $ M.currentSong
  case res of
    Left err -> print err
    Right song -> print song
