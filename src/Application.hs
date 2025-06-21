{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Used otherwise as a pattern" #-}

module Application (startTheApp) where

import Control.Monad (void)
import Lens.Micro (lens, (^.))
import Lens.Micro.Mtl
#if !(MIN_VERSION_base(4,11,0))
import           Data.Monoid
#endif
import Brick
import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.List as L
import Data.Text as TT
import Data.Text.IO as TTIO
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import Lens.Micro.Type
import Other (startOthe)
import Sur (getTodayEntry, startVIMquestionMark)

listL :: Lens' State (L.List () String)
listL = lens _list (\s x -> s {_list = x})

textL :: Lens' State TT.Text
textL = lens _text (\s x -> s {_text = x})

drawUI :: State -> [Widget ()]
drawUI l = [ui]
  where
    label = str "Item " <+> cur <+> str " of " <+> total
    cur = case l ^. listL . L.listSelectedL of
      Nothing -> str "-"
      Just i -> str (show (i + 1))
    total = str $ show $ Vec.length $ l ^. listL . L.listElementsL
    box =
      B.borderWithLabel label $
        hLimit 50 $
          vLimit 3 $
            L.renderList listDrawElement True $
              l ^. listL

    fileContent = l ^. textL
    file =
      hLimit 500 $
        str (TT.unpack $ TT.take 1000 fileContent)
    ui =
        hBox
          [ C.vCenter box,
            B.vBorder,
            C.hCenter file
          ]

appEvent :: T.BrickEvent () e -> T.EventM () State ()
appEvent (T.VtyEvent e) =
  case e of
    V.EvKey V.KEsc [] -> M.halt
    V.EvKey V.KEnter [] -> do
      sel <- use (listL . L.listSelectedL)
      case sel of
        Nothing -> return ()
        Just i -> do
          case i of
            0 -> do
              M.suspendAndResume $ do
                handleToday
                entries <- getTodayEntry
                initialContents <- TTIO.readFile entries
                let initialState = State (L.list () (Vec.fromList ["Select today", "Select Other days", "Quit"]) 1) initialContents
                return initialState
            1 -> do
              M.suspendAndResume $ do
                handleOther
                entries <- getTodayEntry
                initialContents <- TTIO.readFile entries
                let initialState = State (L.list () (Vec.fromList ["Select today", "Select Other days", "Quit"]) 1) initialContents
                return initialState
            2 -> M.halt
            _ -> return ()
    V.EvKey (V.KChar 'q') [] -> M.halt
    ev -> zoom listL $ L.handleListEvent ev
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
    _text :: TT.Text
  }
  deriving (Show)

startTheApp :: IO ()
startTheApp = do
  entries <- getTodayEntry
  initialContents <- TTIO.readFile entries
  let initialState = State (L.list () (Vec.fromList ["Select today", "Select Other days", "Quit"]) 1) initialContents -- Keep your list focus
  void $ M.defaultMain theApp initialState
