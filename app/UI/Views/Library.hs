{-# LANGUAGE OverloadedStrings #-}
module UI.Views.Library
( draw
, event
) where

import ClassyPrelude hiding ((<>))

import qualified UI.Views.Main as Main

import Brick.Types (Widget(..), BrickEvent(..), EventM, Next)
import Brick.Main (continue, halt)
import Lens.Micro.Platform ((^.), (%~), (&), (.~))

import qualified Graphics.Vty as V

import qualified UI.Widgets.Library as Library

import UI.Types (AppState(..), UIName, filteredLibrary, library)

type NextState = EventM UIName (Next AppState)

draw :: AppState -> [Widget UIName]
draw state = Main.draw state widget
  where widget = Library.mkWidget (state^.filteredLibrary) 

-- handleLibraryEvent :: (Ord n) => Event -> Library -> EventM n Library
-- handleLibraryEvent e lib =
--   case e of
--     EvKey KUp [] -> return $ libraryMoveUp lib
--     EvKey KDown [] -> return $ libraryMoveDown lib

event :: AppState -> BrickEvent UIName e -> EventM UIName (Next AppState)
event state (VtyEvent e) = case e of
    (V.EvKey (V.KChar 'j') []) -> next state
    (V.EvKey (V.KChar 'k') []) -> previous state
    -- (V.EvKey V.KEnter []) -> play state
    (V.EvKey (V.KChar 'q') []) -> halt state
    -- VtyEvent (V.EvKey (V.KChar '1') []) -> changeView state 1
    {-V.EvKey (V.KChar '-') [] -> delete-}
    -- ev -> listEvent ev state
    ev -> continue state
event state _ = continue state

next :: AppState -> NextState
next state = continue $ state & filteredLibrary %~ Library.libraryMoveDown

previous :: AppState -> NextState
previous state = continue $ state & filteredLibrary %~ Library.libraryMoveUp

-- libraryEvent :: V.Event -> AppState -> NextState
-- libraryEvent event state = continue =<< (\m -> state & filteredLibrary .~ m) <$> handleLibraryEvent event (state^.library)
