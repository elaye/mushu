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
  , fetchArtistAlbums
  , fetchArtistAlbumSongs
  )

import Network.MPD (Song(..), toString)

import Data.Map.Strict (Map(..), fromDistinctAscList)
import qualified Data.Vector as V

type ArtistName = Text
type AlbumName = Text

data Library = Library
  { _artists :: Map ArtistName (V.Vector AlbumName)
  , _albums :: Map AlbumName (V.Vector Song)
  }

makeSuffixLenses ''Library

fetchLibrary :: IO Library
fetchLibrary = do
  let toText = pack . toString
  artists <- fetchAllArtists
  let fetchSongs ar al = do
        ss <- fetchArtistAlbumSongs ar al
        return (toText al, V.fromList ss)
  let fetchAlbumSongs (ar, als) = mapM (fetchSongs ar) als
  artistsAlbums <- mapM fetchArtistAlbums artists
  albumsSongs <- concat <$> mapM fetchAlbumSongs artistsAlbums

  -- TODO: check that we really have ordered lists of unique keys
  return Library
    { _artists = fromDistinctAscList (map (\(a, b) -> (toText a, V.fromList (map toText b))) artistsAlbums)
    , _albums = fromDistinctAscList albumsSongs
    }
