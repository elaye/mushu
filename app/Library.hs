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
import MPD
  ( fetchAllArtists
  , fetchAllSongs
  , fetchArtistAlbums
  , fetchArtistAlbumSongs
  )

-- import Network.MPD (Song(..), Artist(..), Album(..), toString)
import Network.MPD (Song(..), Metadata(..), toString)

import Data.Map.Strict (Map(..))
import qualified Data.Map.Strict as Map
import Data.Set (Set(..))
import qualified Data.Set as Set
import Data.Vector (Vector(..))
import qualified Data.Vector as V

type ArtistName = Text
type AlbumName = Text

data Library = Library
  -- { _artists :: Map ArtistName (V.Vector AlbumName)
  { _artists :: Map ArtistName (Set AlbumName)
  , _albums :: Map AlbumName (Vector Song)
  }

makeSuffixLenses ''Library

-- fetchLibrary :: IO Library
-- fetchLibrary = do
--   let toText = pack . toString
--   let tag key song = concat (pack <$> toString <$> findWithDefault [fromString ""] key (sgTags song))
--   songs <- fetchAllSongs
--   let eqTag t a b = (tag t a) == (tag t b)
--   let groupedByArtists = groupBy (eqTag Artist) songs
--   let groupedByArtistsAlbums = groupBy (eqTag Album) groupedByArtists

--   -- TODO: check that we really have ordered lists of unique keys
--   return Library
--     { _artists = fromDistinctAscList (map (\(a, b) -> (toText a, V.fromList (map toText b))) artistsAlbums)
--     , _albums = fromDistinctAscList albumsSongs
--     }

fetchLibrary :: IO Library
fetchLibrary = do
  -- fetchLibrary = fetchAllSongs >>= (return . mkLibrary)
  songs <- fetchAllSongs
  return $ mkLibrary songs

mkLibrary :: [Song] -> Library
mkLibrary songs = Library { _artists = artists, _albums = albums }
  where
    -- f (artists, albums) song = insertWith (++) (tag Artist song) (singleton (tag Album song))
    -- (artists, albums) = foldl' f (empty, empty) songs
    (artists, albums) = foldl' songsToLibraryFolder (emptyArtists, emptyAlbums) songs
    emptyArtists = Map.empty :: Map ArtistName (Set AlbumName)
    emptyAlbums = Map.empty :: Map AlbumName (Vector Song)

songsToLibraryFolder ::
  (Map ArtistName (Set AlbumName), Map AlbumName (Vector Song))
  -> Song
  -> (Map ArtistName (Set AlbumName), Map AlbumName (Vector Song))
songsToLibraryFolder (artists, albums) song = (newArtists, albums)
  where
    tag key song = concat (pack <$> toString <$> findWithDefault [fromString ""] key (sgTags song))
    newArtists = insertWith union (tag Artist song) (Set.singleton (tag Album song)) artists

-- fetchLibrary :: IO Library
-- fetchLibrary = do
--   let toText = pack . toString
--   artists <- fetchAllArtists
--   let fetchSongs ar al = do
--         ss <- fetchArtistAlbumSongs ar al
--         return (toText al, V.fromList ss)
--   let fetchAlbumSongs (ar, als) = mapM (fetchSongs ar) als
--   artistsAlbums <- mapM fetchArtistAlbums artists
--   albumsSongs <- concat <$> mapM fetchAlbumSongs artistsAlbums

--   -- TODO: check that we really have ordered lists of unique keys
--   return Library
--     { _artists = fromDistinctAscList (map (\(a, b) -> (toText a, V.fromList (map toText b))) artistsAlbums)
--     , _albums = fromDistinctAscList albumsSongs
--     }
