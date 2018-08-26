module MPD
( currentSong
, togglePlayPause
, fetchPlaylist
, fetchAllSongs
, fetchAllArtists
, fetchArtistAlbums
, fetchArtistAlbumSongs
, fetchStatus
, clearPlaylist
, addArtistToPlaylist
, addAlbumToPlaylist
, addSongToPlaylist
, setVolume
, tag
, mpdReq
) where

import ClassyPrelude

import Control.Exception.Safe (throw)

import Network.MPD
  ( MPD
  , State(..)
  , Status(..)
  , Song(..)
  , Metadata(..)
  , Query
  , Artist
  , Album
  , def
  , withMPD
  , status
  , play
  , pause
  , playlistInfo
  , list
  , search
  , find
  , anything
  , clear
  , findAdd
  , toString
  , (=?), (<&>)
  )
import qualified Network.MPD as M

import Types
import Brick.BChan (BChan(..), writeBChan)

currentSong :: IO (Maybe Song)
currentSong = do
  res <- withMPD M.currentSong
  case res of
    Left err -> print err >> return Nothing
    Right song -> return song

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

mpdReq :: MPD [a] -> IO [a]
mpdReq req = do
  res <- withMPD req
  case res of
    Left err -> print ("MPD error: " ++ (show err)) >> return []
    Right r -> return r

fetchAllArtists :: IO [M.Value]
fetchAllArtists = mpdReq $ list Artist Nothing

fetchArtistAlbums :: Artist -> IO (Artist, [Album])
fetchArtistAlbums artist = do
  albums <- mpdReq $ list Album (Just artist)
  return (artist, albums)

fetchArtistAlbumSongs :: Artist -> Album -> IO [Song]
fetchArtistAlbumSongs artist album = mpdReq req
  where req = search (Artist =? artist M.<&> Album =? album)

fetchAllSongs :: IO [Song]
fetchAllSongs = mpdReq $ search (Artist =? fromString "")
-- fetchAllSongs = mpdReq $ M.find anything

fetchPlaylist :: IO [Song]
fetchPlaylist = mpdReq $ playlistInfo Nothing

fetchStatus :: IO Status
fetchStatus = do
  st <- withMPD status
  case st of
    Left err -> return def
    Right st -> return st

clearPlaylist :: IO ()
clearPlaylist = void $ withMPD clear

toValue = fromString . unpack

addArtistToPlaylist :: Text -> Bool -> IO ()
addArtistToPlaylist artist play = do
  let query = Artist =? toValue artist
  if play then addToPlaylistAndPlay query else addToPlaylist query

addAlbumToPlaylist :: Maybe Text -> Text -> Bool -> IO ()
addAlbumToPlaylist maybeArtist album play = do
  let
    albumQuery = Album =? toValue album
    query = case maybeArtist of
        Just artist -> Artist =? toValue artist M.<&> albumQuery
        Nothing -> albumQuery
  if play then addToPlaylistAndPlay query else addToPlaylist query

addSongToPlaylist :: Text -> Text -> Bool -> IO ()
addSongToPlaylist album song play = do
  let query = Album =? toValue album M.<&> Title =? toValue song
  if play then addToPlaylist query else addToPlaylist query

addToPlaylist :: Query -> IO ()
addToPlaylist query = do
  res <- withMPD $ findAdd query
  case res of
    Left err -> throw MPDException
    Right _ -> return ()

addToPlaylistAndPlay :: Query -> IO ()
addToPlaylistAndPlay query = do
  n <- length <$> fetchPlaylist
  void $ addToPlaylist query
  r <- withMPD $ play (Just n)
  return ()

setVolume :: Int -> IO ()
setVolume volume = void $ withMPD $ M.setVolume volume

tag :: Metadata -> Text -> Song -> Text
tag key def song = concat (pack <$> toString <$> findWithDefault [fromString (unpack def)] key (sgTags song))
