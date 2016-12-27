{-# LANGUAGE OverloadedStrings #-}

module UI
( start
) where

import ClassyPrelude hiding (on, (<>))

import Mail

import qualified UI.Views.Main as Main
import qualified UI.Widgets.Status as Status

import Lens.Micro ((^.))
import Control.Monad (void)
import Data.Monoid
import Data.Maybe (fromMaybe)
import qualified Graphics.Vty as V

import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap as A
import qualified Data.Vector as Vec
import Brick.Types
  ( Widget
  , Padding(..)
  )
import Brick.Widgets.Core
  ( (<+>)
  , str
  , vLimit
  , hLimit
  , vBox
  , withAttr
  , padLeft
  , padRight
  )
import Brick.Util (fg, on)

data ViewType = MailListView | MailView deriving Show

data AppState = AppState
  { mails :: L.List () Mail
  , mail :: Maybe Mail
  , activeView :: ViewType
  }

{-drawUI :: (Show a) => L.List () a -> [Widget ()]-}
drawUI :: AppState -> [Widget ()]
drawUI state@(AppState _ _ activeView) = case activeView of
  MailListView -> drawMailListUI state
  MailView -> drawMailUI state

drawMailListUI :: AppState -> [Widget ()]
drawMailListUI (AppState mails _ _) = Main.draw $ mkMailListWidget mails

mkMailListWidget :: L.List () Mail -> Widget ()
mkMailListWidget mails = L.renderList listDrawElement True mails

drawMailUI :: AppState -> [Widget ()]
drawMailUI (AppState _ mail _) = Main.draw $ mkMailWidget mail

mkMailWidget :: Maybe Mail -> Widget ()
mkMailWidget mail = str $ unpack $ concat $ maybe [] body mail


appEvent :: AppState -> V.Event -> T.EventM () (T.Next AppState)
appEvent state@(AppState _ _ activeView) e = case activeView of
  MailListView -> mailListViewEvent state e
  MailView -> mailViewEvent state e

mailViewEvent :: AppState -> V.Event -> T.EventM () (T.Next AppState)
mailViewEvent state e = case e of
  V.EvKey (V.KChar 'q') [] -> M.continue $ state { mail = Nothing, activeView = MailListView }
  ev -> M.continue state

mailListViewEvent :: AppState -> V.Event -> T.EventM () (T.Next AppState)
mailListViewEvent state@(AppState mails _ _) e =
    case e of
        V.EvKey (V.KChar '-') [] ->
            case mails^.(L.listSelectedL) of
                Nothing -> M.continue state
                Just i -> M.continue $ state { mails = L.listRemove i mails }

        V.EvKey (V.KChar 'j') [] -> M.continue $ state { mails = L.listMoveDown mails }
        V.EvKey (V.KChar 'k') [] -> M.continue $ state { mails = L.listMoveUp mails }

        V.EvKey V.KEnter [] ->
            case mails^.(L.listSelectedL) of
                Nothing -> M.continue state
                Just i -> M.continue $ state { mail = Just ((mails^.(L.listElementsL)) Vec.! i), activeView = MailView }

        V.EvKey (V.KChar 'q') [] -> M.halt state

        {-ev -> M.continue =<< AppState <$> L.handleListEvent ev mails <*> -}
        ev -> M.continue =<< (\m -> state { mails = m } ) <$> L.handleListEvent ev mails

{-listDrawElement :: (Show a) => Bool -> a -> Widget ()-}
{-listDrawElement sel a =-}
    {-let selStr s = if sel-}
                   {-then withAttr customAttr (str $ "<" <> s <> ">")-}
                   {-else str s-}
    {-in C.hCenter $ str "Item " <+> (selStr $ show a)-}

listDrawElement :: Bool -> Mail -> Widget ()
listDrawElement sel mail = C.hCenter $ frm <+> (pad $ selStr sbj)
  where
    selStr s = if sel
      then withAttr customAttr $ str s
      else str s
    pad w = padLeft Max $ padRight Max $ w
    frm = padRight (Pad 3) $ str $ show $ from mail
    sbj = show $ subject mail

initialState :: [Mail] -> AppState
initialState mails = AppState
  { mails = L.list () (Vec.fromList mails) 1
  , mail = Nothing
  , activeView = MailListView
  }

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listAttr,            fg V.white)
    , (L.listSelectedAttr,    V.black `on` V.white)
    , (customAttr,            fg V.cyan)
    , (Status.attr,            fg V.green)
    ]

theApp :: M.App AppState V.Event ()
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          , M.appLiftVtyEvent = id
          }

start :: [Mail] -> IO ()
start mails = void $ M.defaultMain theApp $ initialState mails
