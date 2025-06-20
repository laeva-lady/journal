{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Used otherwise as a pattern" #-}

module Other (startOthe) where

import Control.Monad (void)
import Lens.Micro (lens, (^.))
import Lens.Micro.Mtl
#if !(MIN_VERSION_base(4,11,0))
import           Data.Monoid
#endif
import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import Brick.Types (Widget)
import qualified Brick.Types as T
import Brick.Util (fg, on)
import Brick.Widgets.Border (border)
import qualified Brick.Widgets.Border as B
import Brick.Widgets.Border.Style
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core
  ( hBox,
    hLimit,
    joinBorders,
    str,
    vBox,
    vLimit,
    withAttr,
    withBorderStyle,
    (<+>),
  )
import qualified Brick.Widgets.List as L
import Data.Text as T
import Data.Text.IO as TIO
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import Sur (getListOfEntries, startVIMquestionMark)

drawUI :: State -> [Widget ()]
drawUI l = [ui]
  where
    label = str "Item " <+> cur <+> str " of " <+> total
    cur = case _list l ^. L.listSelectedL of
      Nothing -> str "-"
      Just i -> str (show (i + 1))
    total = str $ show $ Vec.length $ _list l ^. L.listElementsL
    box =
      B.borderWithLabel label $
        hLimit 100 $
          vLimit 1000 $
            L.renderList listDrawElement True $
              _list l
    file =
        B.border $
          hLimit 100 $
            vLimit 1000 $
                str $ show $ Vec.length $ _list l ^. L.listElementsL

    ui =
      joinBorders $
        withBorderStyle unicode $
          border $
            hBox
              [ C.hCenter $
                  vBox
                    [C.hCenter box],
                B.vBorder,
                C.center file
              ]

appEvent :: T.BrickEvent () e -> T.EventM () State ()
appEvent (T.VtyEvent e) =
  case e of
    V.EvKey V.KEsc [] -> M.halt
    V.EvKey V.KEnter [] -> do
      selected <- use (lens _list (\s l -> s {_list = l}) . L.listSelectedL)
      case selected of
        Nothing -> return ()
        Just i -> do
          handler i
    V.EvKey (V.KChar 'q') [] -> M.halt
    ev -> zoom (lens _list (\s l -> s {_list = l})) (L.handleListEvent ev)
  where
    handler :: Int -> T.EventM () State ()
    handler selected =
      case selected of
        0 -> M.halt
        sel ->
          M.suspendAndResume $ do
            entries <- getListOfEntries
            let selection = Vec.fromList entries Vec.! sel
            startVIMquestionMark selection
            contents <- TIO.readFile selection
            return $ State (L.list () (Vec.fromList entries) 1) contents
appEvent _ = return ()

listDrawElement :: (Show a) => Bool -> a -> Widget ()
listDrawElement sel a =
  let selStr s =
        let item = "[  " <> s <> "  ]"
         in if sel
              then withAttr customAttr (str $ "> " <> item)
              else str $ "  " <> item
   in C.hCenter $ selStr (show a)

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

theApp :: M.App State e ()
theApp =
  M.App
    { M.appDraw = drawUI,
      M.appChooseCursor = M.showFirstCursor,
      M.appHandleEvent = appEvent,
      M.appStartEvent = return (),
      M.appAttrMap = const theMap
    }

data State = State
  { _list :: L.List () String,
    _text :: T.Text
  }

startOthe :: IO ()
startOthe = do
  entries <- getListOfEntries
  contents <- TIO.readFile $ Vec.fromList entries Vec.! 1
  let state = State (L.list () (Vec.fromList entries) 1) contents
  void $ M.defaultMain theApp state
