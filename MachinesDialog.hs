{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module MachinesDialog (renderDialog, runRemoteExecutor, AppState(..), getSelectedListElement) where

import Lens.Micro.TH
import Lens.Micro 
import Control.Monad (void)
import Data.Monoid
import qualified Graphics.Vty as V
import Data.Maybe (fromMaybe)
import Control.Monad.Trans

import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap as A
import qualified Brick.Widgets.Edit as E
import qualified Brick.Focus as F
import Brick.Widgets.Core
import qualified Data.Vector as Vec

import Brick.Types
  ( Widget
  , Padding (..)
  , ViewportType (Horizontal, Vertical, Both)
  )

import Brick.Widgets.Core
  ( (<+>)
  , str
  , vLimit
  , padTop
  , padBottom
  , hLimit
  , vBox
  , withAttr
  , viewport
  )

import Brick.Util (fg, on)


data WName = MachineList | SearchTextfield | ReadmeVP deriving (Eq,Ord, Show)

data AppState = AppState
    { _machineList :: L.List WName String
    , _searchTextfield :: E.Editor String WName
    , _focusedWidget :: F.FocusRing WName
    } 
        
makeLenses ''AppState

-- | Widget name identifiers
--type WName = ()

drawUI :: AppState -> [Widget WName]
drawUI st = 
    [ui]
    where
        l = st ^. machineList
        searchInput =  F.withFocusRing (st ^. focusedWidget) E.renderEditor (st ^. searchTextfield)
        label = str "Machine: " <+> cur <+> str " of " <+> total
        cur = case l^.(L.listSelectedL) of
                Nothing -> str "-"
                Just i -> str (show (i + 1))
        total = str $ show $ Vec.length $ l^.(L.listElementsL)
        box = B.borderWithLabel label $
              hLimit 65 $
              vLimit 5 $
              L.renderList listDrawElement True l

        explanation = 
            hLimit 65 $
            vLimit 5 $
            viewport ReadmeVP Both $
                              vBox $ str "Press ctrl-arrow keys to scroll this viewport horizontally and vertically."
                                   : (str <$> [ "Line " <> show i | i <- [2..25::Int] ])

        ui = 
            C.vCenter $ 
            clickable MachineList $
                vBox [ C.hCenter $ hLimit 20 $ vLimit 5 $ padBottom (Pad 2) $ searchInput
                     , C.hCenter box
                     , C.hCenter explanation
                     , str " "
                     , C.hCenter $ str "Press +/- to add/remove list elements."
                     , C.hCenter $ str "Press Esc to exit."
                     ]

-- | appEventWithSource generate the record field appHandleEvent  :: s -> e -> EventM n (Next s) needed
-- to generate the brick app after supplying a source for the machine list (IO [a])
appEventWithSource :: Show a => (String -> IO [a]) -> AppState -> T.BrickEvent WName e -> T.EventM WName (T.Next (AppState))
appEventWithSource datasource st (T.VtyEvent e) =
    let 
        l = st
        search = E.getEditContents (st ^. searchTextfield)
    in case e of

        V.EvKey V.KEsc [] -> M.halt st

        V.EvKey V.KEnter [] ->
            M.suspendAndResume $ do
                (setMachineList st <$> datasource (head search))
                --putStrLn "Suspended. Please enter something and press enter to resume:"
                --ws <- words <$> getLine
                --return (setMachineList st ws)
            --newState <- liftIO (setMachineList st <$> datasource (head search))
            --M.continue newState

        V.EvKey (V.KChar '\t') [] -> M.continue $ st & focusedWidget %~ F.focusNext

        V.EvKey V.KBackTab [] -> M.continue $ st & focusedWidget %~ F.focusPrev

        V.EvKey k [] ->
            case F.focusGetCurrent (st ^. focusedWidget) of
               Just SearchTextfield -> T.handleEventLensed st searchTextfield E.handleEditorEvent e >>= M.continue
               Just MachineList -> T.handleEventLensed st machineList L.handleListEvent e >>= M.continue
               Just ReadmeVP -> 
                   case k of 
                     V.KDown -> M.vScrollBy (M.viewportScroll ReadmeVP) 1 >> M.continue st
                     V.KUp -> M.vScrollBy (M.viewportScroll ReadmeVP) (-1) >> M.continue st
                     _ -> M.continue st
               Nothing -> return st >>= M.continue

appEventWithSource source st _ = M.continue st 

appEvent :: AppState -> T.BrickEvent WName e -> T.EventM WName (T.Next (AppState))
appEvent = appEventWithSource (const $ return ([] :: [String]))
    

listDrawElement :: (Show a) => Bool -> a -> Widget WName
listDrawElement sel a =
    let selStr s = if sel
                   then withAttr customAttr (str $ "<" <> s <> ">")
                   else str s
    in 
        C.hCenter $ 
        clickable MachineList $
        (selStr $ show a)


setMachineList :: Show a =>  AppState -> [a] -> AppState
setMachineList st ls = st &  machineList .~ listMachine where listMachine = L.list MachineList (Vec.fromList $ map show ls) 1

initialStateFromList :: Show a => [a] -> AppState
initialStateFromList ls = setMachineList defaultState ls

defaultState :: AppState
defaultState = 
    AppState 
    { _machineList = L.list MachineList (Vec.fromList []) 1
    , _searchTextfield = (E.editor SearchTextfield (str . unlines) (Just 1) "")
      , _focusedWidget = F.focusRing [SearchTextfield, MachineList, ReadmeVP]
    }

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listAttr,            V.white `on` V.blue)
    , (L.listSelectedAttr,    V.blue `on` V.white)
    --, (L.listSelectedFocusedAttr,    V.black `on` V.yellow)
    , (B.vBorderAttr,            V.white `on` V.green)
    , (E.editAttr,            V.white `on` V.blue)
    , (E.editFocusedAttr,     V.black `on` V.yellow)
    , (customAttr,            fg V.cyan)
    ]

theApp :: M.App AppState e WName
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }

customApp source = M.App { M.appDraw = drawUI
                      , M.appChooseCursor = M.showFirstCursor
                      , M.appHandleEvent = appEventWithSource source
                      , M.appStartEvent = return
                      , M.appAttrMap = const theMap}

renderDialog :: Show a => [a] -> (AppState -> IO ()) -> IO ()
renderDialog l handler = do
    finalState <-  M.defaultMain theApp (initialStateFromList l)
    handler finalState

runRemoteExecutor :: Show a => (String -> IO [a]) -> (AppState -> IO ()) -> IO ()
runRemoteExecutor source handler = do
    finalState <-  M.defaultMain (customApp source) defaultState
    handler finalState



------ UTILITIES ---------
getSelectedListElement :: AppState -> String
getSelectedListElement st = Vec.unsafeIndex machines selected
    where 
        mlist = st ^. machineList
        machines = L.listElements mlist
        (??) = flip fromMaybe
        selected = L.listSelected mlist  ?? 0
