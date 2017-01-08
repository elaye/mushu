{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Library
( mkLibrary
, Library(..)
, ArtistAlbums(..)
) where

import ClassyPrelude

import qualified Network.MPD as M
import Network.MPD ((=?), (<&>))

import qualified Data.Map.Strict as Map

-- data Library = [Artist]

-- data Artist = Artist
--   { _artistName :: Maybe Text
--   , _artistAlbums :: Maybe [Ablum]
--   }

-- data Album = Album
--   { _albumName :: Maybe Text
--   , _albumArtists :: Maybe [Artist]
--   , _albumSongs :: Maybe [M.Song]
--   }

type ArtistName = Text
type AlbumName = Text

data Library = Library (Map ArtistName ArtistAlbums) deriving (Show)

data ArtistAlbums = ArtistAlbums (Map AlbumName (Vector M.Song)) deriving (Show)

-- mpdReq :: M.MPD a -> IO [M.Value]
mpdReq :: M.MPD [a] -> IO [a]
mpdReq req = do
  res <- M.withMPD req
  case res of
    Left err -> (print ("MPD error: " ++ (show err))) >> return []
    Right r -> return r

getArtists :: IO [M.Value]
getArtists = (take 5) <$> (mpdReq $ M.list M.Artist Nothing)

getArtistAlbums :: M.Artist -> IO [M.Value]
getArtistAlbums artist = (take 5) <$> (mpdReq $ M.list M.Album (Just artist))

getArtistAlbumSongs :: M.Artist -> M.Album -> IO [M.Song]
getArtistAlbumSongs artist album = (take 3) <$> (mpdReq req)
  where req = M.search (M.Artist =? artist <&> M.Album =? album)

mkLibrary :: IO Library
mkLibrary = do
  artists <- getArtists
  print artists
  let getAlbums a = fromList <$> (getArtistAlbums a)
  let getSongs ar al = fromList <$> (getArtistAlbumSongs ar al)
  let kvAlbum ar al = do
        ss <- getSongs ar al
        return (pack (M.toString al), ss)
  let kvArtist ar = do
        als <- getAlbums ar
        print als
        -- TODO: check that we really have an ordered list of unique keys
        -- let as = Map.fromDistinctAscList $ mapM (kvAlbum ar) als
        as <- Map.fromDistinctAscList <$> (sequence (map (kvAlbum ar) als))
        return (pack (M.toString ar), ArtistAlbums as)
  -- TODO: check that we really have an ordered list of unique keys
  -- return $ Map.fromDistinctAscList $ mapM kvArtist artists
  library <- Map.fromDistinctAscList <$> (sequence (map kvArtist artists))
  return $ Library library
