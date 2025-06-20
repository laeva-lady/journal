{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Used otherwise as a pattern" #-}

module Application (startTheApp) where

import           Control.Monad              (void)
import           Lens.Micro                 ((^.))
import           Lens.Micro.Mtl
#if !(MIN_VERSION_base(4,11,0))
import           Data.Monoid
#endif
import           Brick
import qualified Brick.AttrMap              as A
import qualified Brick.Main                 as M
import qualified Brick.Types                as T
import           Brick.Widgets.Border       (border)
import qualified Brick.Widgets.Border       as B
import           Brick.Widgets.Border.Style
import qualified Brick.Widgets.Center       as C
import qualified Brick.Widgets.List         as L
import qualified Data.Vector                as Vec
import qualified Graphics.Vty               as V
import           Other                      (startOthe)
import           Sur                        (getTodayEntry, startVIMquestionMark)

drawUI :: (Show a) => L.List () a -> [Widget ()]
drawUI l = [ui]
  where
    label = str "Item " <+> cur <+> str " of " <+> total
    cur = case l ^. L.listSelectedL of
      Nothing -> str "-"
      Just i  -> str (show (i + 1))
    total = str $ show $ Vec.length $ l ^. L.listElementsL
    box =
      B.borderWithLabel label $
        hLimit 50 $
          vLimit 3 $
            L.renderList listDrawElement True l

    ui =
      withBorderStyle unicode $
        border $
          hBox
            [ C.vCenter $
                C.hCenter $
                  vBox
                    [C.hCenter box]
            ]

appEvent :: T.BrickEvent () e -> T.EventM () (L.List () String) ()
appEvent (T.VtyEvent e) =
  case e of
    V.EvKey V.KEsc [] -> M.halt
    V.EvKey V.KEnter [] -> do
      sel <- use L.listSelectedL
      case sel of
        Nothing -> return ()
        Just i -> do
          case i of
            0 -> do
              M.suspendAndResume $ do
                handleToday
                return initialState
            1 -> do
              M.suspendAndResume $ do
                handleOther
                return initialState
            2 -> M.halt
            _ -> return ()
    V.EvKey (V.KChar 'q') [] -> M.halt
    ev -> L.handleListEvent ev
  where
    handleToday :: IO ()
    handleToday = getTodayEntry >>= startVIMquestionMark

    handleOther :: IO ()
    handleOther = do
      startOthe
      return ()
appEvent _ = return ()

listDrawElement :: (Show a) => Bool -> a -> Widget ()
listDrawElement sel a =
  let selStr s =
        let item = "[  " <> s <> "  ]"
         in if sel
              then withAttr customAttr (str item)
              else str item
   in C.hCenter $ selStr (show a)

initialState :: L.List () String
initialState = L.list () (Vec.fromList ["Select today", "Select Other days", "Quit"]) 1

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> A.attrName "custom"

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (L.listAttr, V.white `on` V.black),
      (L.listSelectedAttr, V.black `on` V.black),
      (customAttr, fg V.cyan),
      (customAttr, V.red `on` V.black) -- custom color
    ]

theApp :: M.App (L.List () String) e ()
theApp =
  M.App
    { M.appDraw = drawUI,
      M.appChooseCursor = M.showFirstCursor,
      M.appHandleEvent = appEvent,
      M.appStartEvent = return (),
      M.appAttrMap = const theMap
    }

startTheApp :: IO ()
startTheApp = void $ M.defaultMain theApp initialState
