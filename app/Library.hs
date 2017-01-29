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
  { _artists :: Map ArtistName (Set AlbumName)
  , _albums :: Map AlbumName (Vector Song)
  }

makeSuffixLenses ''Library

fetchLibrary :: IO Library
fetchLibrary = fetchAllSongs >>= (return . mkLibrary)

mkLibrary :: [Song] -> Library
mkLibrary songs = Library { _artists = artists, _albums = albums }
  where
    (artists, albums) = foldl' songsToLibraryFolder (emptyArtists, emptyAlbums) songs
    emptyArtists = Map.empty :: Map ArtistName (Set AlbumName)
    emptyAlbums = Map.empty :: Map AlbumName (Vector Song)

songsToLibraryFolder ::
  (Map ArtistName (Set AlbumName), Map AlbumName (Vector Song))
  -> Song
  -> (Map ArtistName (Set AlbumName), Map AlbumName (Vector Song))
songsToLibraryFolder (artists, albums) song = (newArtists, newAlbums)
  where
    tag key song = concat (pack <$> toString <$> findWithDefault [fromString ""] key (sgTags song))
    newArtists = insertWith union (tag Artist song) (Set.singleton (tag Album song)) artists
    newAlbums = insertWith (++) (tag Album song) (V.singleton song) albums
