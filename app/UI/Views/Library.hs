{-# LANGUAGE GADTs #-}
module UI.Views.Library
( draw
, event
) where

import ClassyPrelude hiding ((<>), on)

import qualified UI.Views.Main as Main

import Brick.Types (Widget(..), BrickEvent(..), EventM, Next)
import Brick.Widgets.Core ((<=>))
import Brick.Widgets.Border (hBorder)
import Brick.Main (continue, halt)
import Lens.Micro.Platform ((^.), (%~), (&), (.~))

import qualified Graphics.Vty as Vty

import qualified UI.Widgets.Library as Library
import qualified UI.Widgets.Filter as Filter
import UI.Widgets.Filter (isActiveL, isFocusedL)

import UI.Types

import Text.Fuzzy (simpleFilter)

draw :: (Show n, Ord n) => AppState n -> [Widget n]
draw state = Main.draw state widget
  where
    fzf = Filter.mkWidget $ state^.filterStateL
    library = Library.mkWidget $ state^.libraryStateL
    widget = if state^.filterStateL.isActiveL then
      fzf <=> hBorder <=> library else library

event :: AppState UIName -> BrickEvent UIName e -> EventM UIName (Next (AppState UIName))
event state (VtyEvent e) = if state^.filterStateL.isFocusedL then
  filterFocusedEvent state e else filterBlurredEvent state e
event state _ = continue state

filterFocusedEvent :: AppState UIName -> Vty.Event -> EventM UIName (Next (AppState UIName))
filterFocusedEvent state e = case e of
    (Vty.EvKey Vty.KEsc []) -> continue $ resetFilter state
    (Vty.EvKey Vty.KEnter []) -> continue (state & filterStateL %~ Filter.blur)
    ev -> do
      newFilterState <- Filter.handleEvent ev $ state^.filterStateL
      let newLibraryState = filterLibrary (Filter.getValue newFilterState) (state^.libraryStateL)
      continue $ state & filterStateL .~ newFilterState
                           & libraryStateL .~ newLibraryState

filterBlurredEvent :: AppState UIName -> Vty.Event -> EventM UIName (Next (AppState UIName))
filterBlurredEvent state e = case e of
    (Vty.EvKey (Vty.KChar 'f') []) -> continue $ state & filterStateL %~ Filter.focus
    (Vty.EvKey (Vty.KChar 'q') []) -> halt state
    (Vty.EvKey Vty.KEsc []) -> continue $ if state^.filterStateL.isActiveL then
      resetFilter state else state
    ev -> do
      newLibraryState <- Library.handleEvent ev $ state^.libraryStateL
      continue $ state & libraryStateL .~ newLibraryState

resetFilter :: AppState n -> AppState n
resetFilter state = state & filterStateL %~ Filter.reset
                    & libraryStateL %~ Library.resetFilter

filterLibrary :: Text -> Library.LibraryState n -> Library.LibraryState n
filterLibrary token = Library.applyFilter ft
  where ft xs = pack <$> simpleFilter (unpack token) (unpack <$> xs)
