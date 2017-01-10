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

import qualified Data.Text.Zipper as TZ

import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap as A
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.Dialog as D
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
  , withDefAttr 
  , forceAttr
  , withBorderStyle
  , overrideAttr
  , viewport
  )

import Brick.Util (bg, fg, on)

-- | Widget name identifiers
data WName = MachineList | SearchTextfield | ReadmeVP | SideBarOptions | NoWidget deriving (Eq,Ord, Show)

data Action = RunScriptOnAll | RunScript deriving (Show, Eq, Enum)

data AppState = AppState
    { _machineList :: L.List WName String
    , _searchTextfield :: E.Editor String WName
    , _focusedWidget :: F.FocusRing WName
    , _actionDialog :: D.Dialog Action
    , _sideBarOptionList :: L.List WName String
    } 
        
makeLenses ''AppState

------------------------- VIEW  - DRAWING FUNCTIONS --------------------------
drawUI :: AppState -> [Widget WName]
drawUI st = 
    [ui]
    where
        fw = F.focusGetCurrent (st ^. focusedWidget)
        l = st ^. machineList
        optionList = st ^. sideBarOptionList
        d = st ^. actionDialog
        ds = D.dialogSelection d
        focusedWidgetIs n =  fmap (== n) fw ?? False
        overriderAttrWithExt att ext = overrideAttr att (att <> ext)
        customBorder ext b w = 
            if focusedWidgetIs b then overriderAttrWithExt ext B.borderAttr w else withAttr (B.borderAttr) w

        highlightBorder = customBorder "highlighted"
    
        searchInput =  
            let searchInput' = F.withFocusRing (st ^. focusedWidget) E.renderEditor (st ^. searchTextfield)
             in 
               hLimit 65 $
               vLimit 5 $
               (str "Search machines: " <+> (hLimit 30 $ padBottom (Pad 2) $ searchInput'))

        label = str "Machine: " <+> cur <+> str " of " <+> total
        cur = case l^.(L.listSelectedL) of
                Nothing -> str "-"
                Just i -> str (show (i + 1))

        total = str $ show $ Vec.length $ l^.(L.listElementsL)

        sideBarOptions = 
            withDefAttr "options" $
            C.hCenter $
            padBottom (Pad 1) $
            padTop (Pad 1) $
            hLimit 40 $
            vLimit 15 $
            clickable SideBarOptions $
            L.renderList listDrawElement True optionList

        mList = 
            --highlightBorder MachineList $
            withDefAttr "list" $
            B.borderWithLabel label $
            hLimit 65 $
            vLimit 20 $
            clickable MachineList $
            --L.renderList listDrawElement (focusedWidgetIs MachineList) l
            L.renderList listDrawElement True l


        explanation = 
            padAll 2 $
            hLimit 40 $
            vLimit 5 $
            viewport ReadmeVP Vertical $
            clickable ReadmeVP $
            vBox $ (str <$> [show fw , show ds, show $ getSelectedListElement st])

        machineSearch = 
            withDefAttr "machines" $
            C.hCenter $
            D.renderDialog d $
            --withBorderStyle BS.unicodeRounded $ 
            padAll 1 $
            C.center $ 
                vBox [ C.hCenter searchInput
                     , C.hCenter mList
                     , str " "
                     , C.hCenter $ str "Press Esc to exit."
                     ]
        sideBar = 
            withDefAttr "sidebar" $
            hLimit 40 $
            vBox [explanation, sideBarOptions, C.hCenter $ str dydAscii, padTop (Pad 5) $ C.hCenter $ str "Dinámica y Desarrollo"]
            
                     
        verticalSeparator =  
            withDefAttr "separator" $
            hLimit 2 $ 
            withBorderStyle (BS.borderStyleFromChar '*') B.vBorder
        
        ui = 
            C.center $ 
            --withDefAttr "main" $
            overriderAttrWithExt "main" B.borderAttr $
            B.borderWithLabel (str "Remote Machine Process") $
            hLimit 125 $
            hBox [sideBar, verticalSeparator, machineSearch]



------------------------------ EVENT HANDLERS ---------------------------------

-- | appEventWithSource generate the record field appHandleEvent  :: s -> e -> EventM n (Next s) needed
-- to generate the brick app after supplying a source for the machine list (IO [a])


appEventWithSource :: (String -> IO [String]) -> AppState -> T.BrickEvent WName e -> T.EventM WName (T.Next (AppState))

--appEventWithSource source st (T.MouseDown n _ _ loc) = M.continue (st & focusedWidget %~ moveFocus SearchTextfield)
appEventWithSource source st (T.MouseDown n _ _ loc) = M.continue (st & searchTextfield %~ ( E.applyEdit $ TZ.insertChar 'X'))
appEventWithSource source st (T.MouseUp _ _ _) = M.continue $ st

appEventWithSource datasource st (T.VtyEvent e) =
    let 
        fw = F.focusGetCurrent (st ^. focusedWidget)
        selectedMachine = getSelectedListElement st
        --d = st ^. actionDialog
        ds = D.dialogSelection (st ^. actionDialog)
        search = E.getEditContents (st ^. searchTextfield)
    in case e of

        V.EvKey V.KEsc [] -> M.halt defaultState

        V.EvKey V.KEnter [] -> 
            case fw of
               Just SearchTextfield -> M.suspendAndResume (setMachineList st <$> datasource (head search))
               _ -> 
                    case (selectedMachine, ds) of 
                        (Just m, Just n) -> M.halt st
                        (Nothing, _) -> M.continue st

        V.EvKey V.KLeft [] -> 
            T.handleEventLensed st actionDialog D.handleDialogEvent (V.EvKey V.KLeft []) >>= M.continue

        V.EvKey V.KRight [] -> 
            T.handleEventLensed st actionDialog D.handleDialogEvent (V.EvKey V.KLeft []) >>= M.continue

        V.EvKey (V.KChar '\t') [] -> M.continue $ st & focusedWidget %~ F.focusNext

        V.EvKey V.KBackTab [] -> M.continue $ st & focusedWidget %~ F.focusPrev

        V.EvKey k [] ->
            case fw of
               Just SearchTextfield -> T.handleEventLensed st searchTextfield E.handleEditorEvent e >>= M.continue
               Just MachineList -> T.handleEventLensed st machineList L.handleListEvent e >>= M.continue
               Just SideBarOptions -> T.handleEventLensed st sideBarOptionList L.handleListEvent e >>= M.continue
               Just ReadmeVP -> 
                   case k of 
                     V.KDown -> M.vScrollBy (M.viewportScroll ReadmeVP) 1 >> M.continue st
                     V.KUp -> M.vScrollBy (M.viewportScroll ReadmeVP) (-1) >> M.continue st
                     _ -> M.continue st

               Nothing ->
                    T.handleEventLensed st actionDialog D.handleDialogEvent (V.EvKey V.KLeft []) >>= M.continue

appEventWithSource source st _ = M.continue st 

appEvent :: AppState -> T.BrickEvent WName e -> T.EventM WName (T.Next (AppState))
appEvent = appEventWithSource (const $ return ([] :: [String]))
    

------------------------- INITIAL STATE -------------------------

initialStateFromList :: [String] -> AppState
initialStateFromList ls = setMachineList defaultState ls

defaultActionDialog :: D.Dialog Action
defaultActionDialog = 
    defaultDialog & D.dialogButtonsL .~ choices
    where
        defaultDialog = D.dialog (Just "Dyd Remote Executor") Nothing 70
        choices = [ ("Run on all", RunScriptOnAll)
                  , ("Run", RunScript)
                  ]

defaultState :: AppState
defaultState = 
    AppState 
    { _machineList = L.list MachineList (Vec.fromList []) 1
    , _searchTextfield = (E.editor SearchTextfield (str . unlines) (Just 1) "")
    , _focusedWidget = F.focusRing [SideBarOptions, SearchTextfield, MachineList, ReadmeVP]
    , _actionDialog = defaultActionDialog
    , _sideBarOptionList = L.list SideBarOptions (Vec.fromList ["Run script","Search Machines"]) 1
    }


---------------------- ATTRIBUTES --------------------------

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

mintColor = V.rgbColor 95 186 125

beige = V.rgbColor 221 221 166
olive = V.rgbColor 150 160  11
orange = V.rgbColor 186 94 14
darkBlue = V.rgbColor 0 82 89

globalDefault = V.white `on` V.black

theMap :: A.AttrMap
theMap = A.attrMap globalDefault
    [ (L.listAttr, V.white `on` V.blue)
    , ("options" , bg V.black)
    , ("main" , bg V.white)
    , ("button" , bg V.yellow)
    --, ("machines", V.red `on` V.black) 
    --, ("list", V.red `on` V.black) 
    --, ("sidebar", V.black `on` V.white) 
    , (B.borderAttr <> "highlighted",  fg V.red)
    , (B.borderAttr <> "main",  fg V.red)
    , (L.listSelectedAttr, V.black `on` V.white)
    , (D.dialogAttr, fg V.white)
    , (D.buttonAttr, V.black `on` V.white)
    , (D.buttonSelectedAttr, bg V.magenta)
    , ("separator" ,  darkBlue `on` darkBlue)
    , (E.editAttr,            V.white `on` V.blue)
    , (E.editFocusedAttr,     V.black `on` V.yellow)
    , (customAttr,            fg V.red)
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

-- | renderDialog Just tackes a list of strings to populate the list widget
renderDialog :: [String] -> (AppState -> IO ()) -> IO ()
renderDialog l handler = do
    finalState <-  M.defaultMain theApp (initialStateFromList l)
    handler finalState

-- | runRemoteExecutor runs an IO action that takes as parameter the search string of the edit box and returns a list to 
-- populate the list widget
runRemoteExecutor :: (String -> IO [String]) -> (AppState -> IO ()) -> IO ()
runRemoteExecutor source handler = do
    finalState <-  M.defaultMain (customApp source) defaultState
    handler finalState


--------- HELPERS ----------

listDrawElement :: (Show a) => Bool -> a -> Widget WName
listDrawElement sel a =
    let selStr s = if sel
                   then withAttr customAttr (str $ "<" <> s <> ">")
                   else str s
    in 
        C.hCenter $ 
        clickable MachineList $
        (selStr $ show a)

moveFocus n fr = 
        case F.focusGetCurrent fr of
            Nothing -> fr
            Just m -> if m == n then fr else moveFocus n (F.focusNext fr)

mkButton (name, label, attr) =
			clickable name $
		    withDefAttr attr $
            B.border $
            padTopBottom 1 $
            str label
            

------ UTILITIES ---------

(??) = flip fromMaybe

setMachineList ::  AppState -> [String] -> AppState
setMachineList st ls = st &  machineList .~ listMachine where listMachine = L.list MachineList (Vec.fromList  ls) 1

getSelectedListElement :: AppState -> Maybe String
getSelectedListElement st = snd <$> selected 
    where 
        mlist = st ^. machineList
        selected = L.listSelectedElement mlist

dydAscii = ":::::::::  :::   ::: :::::::::  \n:+:    :+: :+:   :+: :+:    :+: \n+:+    +:+  +:+ +:+  +:+    +:+ \n+#+    +:+   +#++:   +#+    +:+ \n+#+    +#+    +#+    +#+    +#+ \n#+#    #+#    #+#    #+#    #+# \n#########     ###    #########  "

