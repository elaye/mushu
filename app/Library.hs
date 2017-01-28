{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Library
( fetchLibrary
, Library(..)
, ArtistName
, AlbumName
, artistsL
, albumsL
) where

import ClassyPrelude

import TH (makeSuffixLenses)

import Lens.Micro.Platform ((^.), (&), (.~))

import qualified Network.MPD as M
import Network.MPD ((=?), (<&>))

import Data.Map.Strict (Map(..), fromDistinctAscList)
import qualified Data.Vector as V
import Data.Vector ((!))

type ArtistName = Text
type AlbumName = Text

data Library = Library
  { _artists :: Map ArtistName (V.Vector AlbumName)
  , _albums :: Map AlbumName (V.Vector M.Song)
  }

makeSuffixLenses ''Library

-- mpdReq :: M.MPD a -> IO [M.Value]
mpdReq :: M.MPD [a] -> IO [a]
mpdReq req = do
  res <- M.withMPD req
  case res of
    Left err -> (print ("MPD error: " ++ (show err))) >> return []
    Right r -> return r

getArtists :: IO [M.Value]
-- getArtists = (take 5) <$> (mpdReq $ M.list M.Artist Nothing)
getArtists = mpdReq $ M.list M.Artist Nothing

-- getArtistAlbums :: M.Artist -> IO [M.Value]
-- -- getArtistAlbums artist = (take 5) <$> (mpdReq $ M.list M.Album (Just artist))
-- getArtistAlbums artist = mpdReq $ M.list M.Album (Just artist)

getArtistAlbumSongs :: M.Artist -> M.Album -> IO [M.Song]
-- getArtistAlbumSongs artist album = (take 3) <$> (mpdReq req)
getArtistAlbumSongs artist album = mpdReq req
  where req = M.search (M.Artist =? artist <&> M.Album =? album)

fetchArtistAlbums :: M.Artist -> IO (M.Artist, [M.Album])
fetchArtistAlbums artist = do
  albums <- mpdReq $ M.list M.Album (Just artist)
  return (artist, albums)

fetchLibrary :: IO Library
-- fetchLibrary :: n -> IO (Library n)
fetchLibrary = do
  let toText = pack . M.toString
  artists <- getArtists
  -- print artists
  -- let getAlbums a = fromList <$> (getArtistAlbums a)
  -- let getSongs ar al = fromList <$> (getArtistAlbumSongs ar al)
  let fetchSongs ar al = do
        -- ss <- getSongs ar al
        ss <- getArtistAlbumSongs ar al
        return (toText al, V.fromList ss)
  let kvAlbum (ar, als) = sequence $ map (fetchSongs ar) als
  -- let kvAlbum aAls = map fetchSongs aAls

  -- let kvArtist ar = do
  --       als <- getAlbums ar
  --       -- print als
  --       -- TODO: check that we really have an ordered list of unique keys
  --       -- let as = Map.fromDistinctAscList $ mapM (kvAlbum ar) als
  --       -- as <- Map.fromDistinctAscList <$> (sequence (map (kvAlbum ar) als))
  --       -- return (pack (M.toString ar), (V.fromList als))
  --       return (pack (M.toString ar), (map (pack . M.toString) als))

  -- TODO: check that we really have an ordered list of unique keys
  -- return $ Map.fromDistinctAscList $ mapM kvArtist artists
  -- artistsAlbums <- sequence $ map kvArtist artists
  artistsAlbums <- sequence $ map fetchArtistAlbums artists
  -- let allAlbums = concat $ snd <$> kvArtistAlbums

  -- albumsSongs <- sequence $ map kvAlbum artistsAlbums
  -- albumsSongs <- sequence $ map kvAlbum artistsAlbums
  albumsSongs <- concat <$> (sequence $ map kvAlbum artistsAlbums)


  return $ Library
    { _artists = fromDistinctAscList (map (\(a, b) -> (toText a, V.fromList (map toText b))) artistsAlbums)
    , _albums = fromDistinctAscList albumsSongs
    }

-- fetchLibrary ::IO Library
-- -- fetchLibrary :: n -> IO (Library n)
-- fetchLibrary name = do
--   artists <- getArtists
--   -- print artists
--   let getAlbums a = fromList <$> (getArtistAlbums a)
--   let getSongs ar al = fromList <$> (getArtistAlbumSongs ar al)
--   let kvAlbum ar al = do
--         ss <- getSongs ar al
--         return (pack (M.toString al), (L.list "list-album-songs" ss 1))
--   let kvArtist ar = do
--         als <- getAlbums ar
--         -- print als
--         -- TODO: check that we really have an ordered list of unique keys
--         -- let as = Map.fromDistinctAscList $ mapM (kvAlbum ar) als
--         as <- Map.fromDistinctAscList <$> (sequence (map (kvAlbum ar) als))
--         return (pack (M.toString ar), ArtistAlbums as (V.fromList (Map.keys as)) (Just 0))
--   -- TODO: check that we really have an ordered list of unique keys
--   -- return $ Map.fromDistinctAscList $ mapM kvArtist artists
--   artistsAlbums <- Map.fromDistinctAscList <$> (sequence (map kvArtist artists))
--   return $ Library
--     { _artists = artistsAlbums
--     , _albums = (V.fromList (Map.keys artistsAlbums)) (Just 0) name
--     }

