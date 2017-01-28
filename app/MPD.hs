module MPD
( currentSong
, togglePlayPause
) where

import ClassyPrelude

import Network.MPD
  ( State(..)
  , Status(..)
  , withMPD
  , status
  , play
  , pause
  )
import qualified Network.MPD as M

currentSong :: IO ()
currentSong = do
  res <- withMPD $ M.currentSong
  case res of
    Left err -> print err
    Right song -> print song

togglePlayPause :: IO ()
togglePlayPause = do
  resStatus <- withMPD status
  case resStatus of
    Left _ -> return ()
    Right st -> do
      case (stState st) of
        Playing -> void $ withMPD $ pause True
        Stopped -> void $ withMPD $ play Nothing
        Paused -> void $ withMPD $ pause False
