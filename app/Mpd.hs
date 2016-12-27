{-# LANGUAGE OverloadedStrings #-}
module Mpd
( connect
) where

import ClassyPrelude
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as C
import Network.Socket
  ( socket
  , Family(..)
  , AddrInfo(..)
  , AddrInfoFlag(..)
  , SocketType(..)
  , defaultHints
  , getAddrInfo
  , Socket(..)
  , addrFamily
  , close
  )
import qualified Network.Socket as N

connect :: Text -> Text -> IO ()
connect ip port = do
  let hints = defaultHints
        { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV]
        , addrSocketType = Stream
        }
  -- addr:_ <- getAddrInfo (Just hints) (Just "127.0.0.1") (Just "5000")
  addr:_ <- getAddrInfo (Just hints) (Just (unpack ip)) (Just (unpack port))
  sock@(MkSocket _ fam stype _ _) <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  print fam
  print stype
  N.connect sock (addrAddress addr)
  -- sendAll sock $ C.pack ""
  -- print nBytesSent
  msg <- recv sock 1024
  putStr "Received: "
  C.putStr msg

  sendAll sock $ C.pack "play\n"

  close sock
