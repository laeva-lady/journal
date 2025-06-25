{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Used otherwise as a pattern" #-}

module Other (startOthe) where

import           Control.Monad          (void, when)
import           Lens.Micro             (lens, (^.))
import           Lens.Micro.Mtl         (use, zoom, (.=))
#if !(MIN_VERSION_base(4,11,0))
import           Data.Monoid
#endif
import qualified Brick.AttrMap          as A
import qualified Brick.Main             as M
import           Brick.Types            (Widget)
import qualified Brick.Types            as T
import           Brick.Util             (on)
import qualified Brick.Widgets.Border   as B
import qualified Brick.Widgets.Center   as C
import           Brick.Widgets.Core
import qualified Brick.Widgets.List     as L
import           Control.Monad.IO.Class
import           Data.List              (sortBy)
import           Data.Ord
import           Data.Text              as T
import           Data.Text.IO           as TIO
import qualified Data.Vector            as Vec
import qualified Graphics.Vty           as V
import           Lens.Micro.Type
import           Sur                    (getListOfEntries, startVIMquestionMark)

-- State definition
data State = State
  { _list :: L.List () String,
    _text :: T.Text
  }

-- Lenses for easier state access (optional but good practice with lens)
listL :: Lens' State (L.List () String)
listL = lens _list (\s x -> s {_list = x})

textL :: Lens' State T.Text
textL = lens _text (\s x -> s {_text = x})

drawUI :: State -> [Widget ()]
drawUI s = [ui]
  where
    label = str "Item " <+> cur <+> str " of " <+> total
    cur = case s ^. listL . L.listSelectedL of
      Nothing -> str "-"
      Just i  -> str (show (i + 1))
    total = str $ show $ Vec.length $ s ^. listL . L.listElementsL

    box =
      B.borderWithLabel label $
        hLimit 50 $
          vLimit 1000 $
            L.renderList listDrawElement True $
              s ^. listL

    fileContent = s ^. textL
    file =
      hLimit 1000 $
        vLimit 1000 $
          str $ T.unpack fileContent
    ui =
      hBox
        [ C.vCenter box,
          B.vBorder,
          file
        ]

appEvent :: T.BrickEvent () e -> T.EventM () State ()
appEvent (T.VtyEvent e) =
  case e of
    V.EvKey V.KEsc [] -> M.halt
    V.EvKey V.KEnter [] -> do
      selectedIdx <- use (listL . L.listSelectedL)
      M.suspendAndResume $ do
        entries <- liftIO getListOfEntries
        case selectedIdx of
          Nothing -> return ()
          Just idx -> do
            let arts = Vec.fromList (sortBy (comparing Data.Ord.Down) entries) Vec.! idx
            liftIO $ startVIMquestionMark arts
        entries' <- getListOfEntries
        initialContents <-
          if Vec.null (Vec.fromList entries)
            then return T.empty
            else TIO.readFile $ Vec.fromList entries' Vec.! 0
        let newState = State (L.list () (Vec.fromList $ sortBy (comparing Data.Ord.Down) entries') 1) initialContents
        return newState
    V.EvKey (V.KChar 'q') [] -> M.halt
    ev -> do
      zoom listL $ L.handleListEvent ev
      mSel <- use (listL . L.listSelectedL)
      elements <- use (listL . L.listElementsL)
      case mSel of
        Just i ->
          when (i < Vec.length elements) $
            do
              let path = elements Vec.! i
              content <- liftIO $ TIO.readFile path
              textL .= content
        _ -> return ()
appEvent _ = return ()

listDrawElement :: (Show a) => Bool -> a -> Widget ()
listDrawElement sel a =
  let selStr s =
        if sel
          then withAttr customAttr (str $ "> " <> s)
          else str $ "  " <> s
   in C.hCenter $ selStr (show a)

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> A.attrName "custom"

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (L.listAttr, V.white `on` V.black),
      (L.listSelectedAttr, V.black `on` V.black),
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

startOthe :: IO ()
startOthe = do
  entries <- getListOfEntries
  -- Ensure there's at least one entry before trying to read it
  initialContents <-
    if Vec.null (Vec.fromList entries)
      then return T.empty
      else TIO.readFile $ Vec.fromList entries Vec.! 0 -- Start with the first entry (index 0)
  let initialState = State (L.list () (Vec.fromList $ sortBy (comparing Data.Ord.Down) entries) 1) initialContents -- Keep your list focus
  void $ M.defaultMain theApp initialState
