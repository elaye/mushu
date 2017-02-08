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
, addToPlaylist
, addToPlaylistAndPlay
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
  where req = search (Artist =? artist <&> Album =? album)

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

mkQuery :: (Maybe Text, Maybe Text, Maybe Text) -> Maybe Query
mkQuery (Nothing, _, _) = Nothing
mkQuery (Just artist, maybeAlbum, maybeTitle) = Just queryArtistAlbumTitle
  where
    toValue = fromString . unpack
    queryArtist = Artist =? (toValue artist)
    queryArtistAlbum = case maybeAlbum of
      Just album -> queryArtist <&> Album =? (toValue album)
      Nothing -> queryArtist
    queryArtistAlbumTitle = case maybeTitle of
      Just title -> queryArtistAlbum <&> Title =? (toValue title)
      Nothing -> queryArtistAlbum

addToPlaylist :: (Maybe Text, Maybe Text, Maybe Text) -> IO ()
addToPlaylist selection = do
  let query = mkQuery selection
  case query of
    Nothing -> print "Missing artist" >> return ()
    Just q -> do
      res <- withMPD $ findAdd q
      case res of
        Left err -> throw MPDException
        Right _ -> return ()

addToPlaylistAndPlay :: (Maybe Text, Maybe Text, Maybe Text) -> IO ()
addToPlaylistAndPlay (Nothing, _, _) = print "unknown artist for adding to playlist"
addToPlaylistAndPlay selection = do
  n <- length <$> fetchPlaylist
  void $ addToPlaylist selection
  r <- withMPD $ play (Just n)
  return ()

setVolume :: Int -> IO ()
setVolume volume = void $ withMPD $ M.setVolume volume

tag :: Metadata -> Text -> Song -> Text
tag key def song = concat (pack <$> toString <$> findWithDefault [fromString (unpack def)] key (sgTags song))
