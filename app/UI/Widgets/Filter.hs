{-# LANGUAGE TemplateHaskell #-}
module UI.Widgets.Filter
( FilterState
, mkState
, mkWidget
, handleEvent
, attrs
, isActiveL
, isFocusedL
, focus
, blur
, reset
, getValue
) where

import ClassyPrelude
import TH (makeSuffixLenses)

import Lens.Micro.Platform ((^.), (%~), (&), (.~))

import Data.Text.Zipper (textZipper)

import Brick.Types (Widget, BrickEvent, EventM, Next, Padding(..))
import Brick.AttrMap (AttrName)
import Brick.Widgets.Core (str, withAttr, padLeft, (<+>))
import Brick.Util (fg)
import Brick.Widgets.Edit
  ( Editor
  , editorText
  , applyEdit
  , renderEditor
  , handleEditorEvent
  , getEditContents
  , editAttr
  )

import qualified Graphics.Vty as Vty

data FilterState n = FilterState
  { _editor :: Editor Text n
  , _isFocused :: Bool
  , _isActive :: Bool
  }

makeSuffixLenses ''FilterState

mkState :: n -> FilterState n
mkState name = FilterState
  { _editor = editorText name (str . (concatMap unpack)) (Just 1) ""
  , _isFocused = False
  , _isActive = False
  }

mkWidget :: (Show n, Ord n) => FilterState n -> Widget n
mkWidget state = str "Filter: " <+> (withAttr filterAttrName (renderEditor (state^.isFocusedL) (state^.editorL))) <+> (padLeft Max (str "Enter: apply | Esc: disable "))

handleEvent :: Vty.Event -> FilterState n -> EventM n (FilterState n)
handleEvent event state = do
  newEditorState <- handleEditorEvent event (state^.editorL)
  return $ state & editorL .~ newEditorState

focus :: FilterState n -> FilterState n
focus state = state & isActiveL .~ True
                    & isFocusedL .~ True

blur :: FilterState n -> FilterState n
blur state = state & isFocusedL .~ False

reset :: FilterState n -> FilterState n
reset state = state & isActiveL .~ False
                    & isFocusedL .~ False
                    & editorL %~ clearEditor

getValue :: FilterState n -> Text
getValue state = concat $ getEditContents (state^.editorL)

filterAttrName :: AttrName
filterAttrName = editAttr

clearEditor :: Editor Text n -> Editor Text n
clearEditor editor = applyEdit (\_ -> textZipper [""] (Just 1)) editor

attrs :: [(AttrName, Vty.Attr)]
attrs = [ (filterAttrName, fg Vty.yellow)
        ]
