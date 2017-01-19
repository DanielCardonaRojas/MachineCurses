{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}

module MachinesDialog (renderDialog, runRemoteExecutor, AppState(..), getSelectedMachine) where

import Lens.Micro.TH
import Lens.Micro 
import Control.Monad (void)
import Data.Monoid
import qualified Graphics.Vty as V
import Data.Maybe (fromMaybe)
import Data.List (intersperse, elemIndex)
import Control.Monad.Trans

import qualified Data.Text.Zipper as TZ
import Data.String (fromString)

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
  , padLeft
  , padRight
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

{-
TODO: 
    - Move page rendering functions to separete modules.

    - Make the generalize the machine list to a list of tuples and select which information to see 
      maybe with toggle boxes below the searchbar.

    - Add keydingins to navigate with jk (move up and down the list) lh move selected info toggle and space to toggle selection.

-}

-- | Widget name identifiers
data WName = MachineList | SearchTextfield | CommandTextfield | ReadmeVP | SideBarOptions | Button Page | Toggles | Toggle InfoToggle | NoWidget 
    deriving (Eq, Ord)

deriving instance Show WName

data Page = PageCustom | PageInfo | PageDiagnostics | PageConfirmation deriving (Eq,Ord, Enum, Bounded)

data InfoToggle = InfoVPN | InfoHost deriving (Show, Enum, Bounded, Ord, Eq)

data Action = OK | Cancel | Quit | RunScriptOnAll | RunScript deriving (Show, Eq, Enum)


data AppState = AppState
    { _currentPage :: Page 
    , _machineList :: L.List WName String
    , _searchTextfield :: E.Editor String WName
    , _commandTextfield :: E.Editor String WName
    , _focusedWidget :: F.FocusRing WName
    , _pageDialog :: D.Dialog Action
    , _infoToggles :: [(InfoToggle, Bool)]
    } 
        
makeLenses ''AppState

instance Show Page where
    show p = 
        case p of
          PageCustom -> "Custom Bash"
          PageInfo -> "Info"
          PageDiagnostics -> "Diagnostico"
          PageConfirmation -> "Confirmación"
          _ -> "Page Title"

------------------------- VIEW  - DRAWING FUNCTIONS --------------------------
drawUI :: AppState -> [Widget WName]
drawUI st = 
    let
        fw = F.focusGetCurrent (st ^. focusedWidget)
        d = st ^. pageDialog
        ds = D.dialogSelection d
        focusedWidgetIs n =  fmap (== n) fw ?? False
        searchString = mconcat $ E.getEditContents (st ^. searchTextfield)
        commandString = mconcat $ E.getEditContents (st ^. commandTextfield)
        overriderAttrWithExt att ext = overrideAttr att (att <> ext)
        customBorder ext b w = 
            if focusedWidgetIs b then overriderAttrWithExt ext B.borderAttr w else withAttr (B.borderAttr) w

        highlightBorder = customBorder "highlighted"
    
        sideBarOptions = C.hCenter $ padding 1 1 5 0 $ vBox (intersperse (str " ") buttons)
            where 
                allPages = [PageCustom .. (pred PageConfirmation)]
                buttons = map mkButton $ zip3 (Button <$> allPages) (show <$> allPages) ((fromString . ("button" ++) . show) <$> [1..]) 

        explanation = 
            padding 6 0 3 2 $
            hLimit 40 $
            vLimit 5 $
            viewport ReadmeVP Vertical $
            clickable ReadmeVP $
                vBox $ (str <$> [commandString ++ " " ++ searchString, show fw 
                                , show ds
                                , show $ getSelectedMachine st
                                , show $ st ^. (pageDialog . D.dialogButtonsL) 
                                , show $ st ^. currentPage
                                , aboutText])

        sideBar = 
            withDefAttr "sidebar" $
            hLimit 40 $
            vBox [explanation
                 , sideBarOptions
                 , C.hCenter $ str dydAscii
                 , padTop (Pad 1) $ C.hCenter $ str "Dinámica y Desarrollo"
                 , C.hCenter $ str "Press Esc to exit." 
                 ]
            
                     
        verticalSeparator =  
            withDefAttr "separator" $
            hLimit 2 $ 
            withBorderStyle (BS.borderStyleFromChar '*') B.vBorder
        
        sidebarWithPage page = 
            C.center $ 
            --withDefAttr "main" $
            overriderAttrWithExt "main" B.borderAttr $
            B.borderWithLabel (str "Remote Machine Process") $
            hLimit 115 $
            hBox [verticalSeparator, sideBar, verticalSeparator, page]
        in
            case st ^. currentPage of
              PageCustom -> [sidebarWithPage $ machinePageView st]
              PageConfirmation -> [sidebarWithPage $ confirmationPage st]
              PageInfo -> [sidebarWithPage $ infoPage st]
              PageDiagnostics -> [sidebarWithPage $ diagnosticsPage st]
              _ -> [sidebarWithPage $ defaultPage st]
                  
				

------------ PAGES ---------------

infoPage st = withMachineSearch (str "Info widgets missing") st

diagnosticsPage st = withMachineSearch (str "Diagnostics widgets missing") st

defaultPage :: AppState -> Widget WName 
defaultPage st = 
    mkPage Nothing st $ str "Not implemented page"

machinePageView :: AppState -> Widget WName 
machinePageView st = 
    let
        commandInput =  
            let commandinput = F.withFocusRing (st ^. focusedWidget) E.renderEditor (st ^. commandTextfield)
            in 
               vLimit 10 $
               (str "Enter a shell command" <=> (clickable CommandTextfield $  padBottom (Pad 2) $ commandinput))

    in
        withMachineSearch commandInput st

confirmationPage :: AppState -> Widget WName 
confirmationPage st =
    mkPage Nothing st $ str "Confirmation Page"

withMachineSearch :: Widget WName -> AppState ->  Widget WName 
withMachineSearch wid st = 
    let

        searchInput =  
            let searchInput' = F.withFocusRing (st ^. focusedWidget) E.renderEditor (st ^. searchTextfield)
             in 
               hLimit 65 $
               vLimit 5 $
               (str "Buscar : " <+> (clickable SearchTextfield $ hLimit 30 $ padBottom (Pad 2) $ searchInput'))

        
        toggles  = 
            --highlightBorder MachineList $
            withDefAttr "toggles" $
            vLimit 8 $
            hLimit 65 $
            clickable Toggles $
            renderHorizontalList listDrawToggles (st ^. infoToggles)

        mList = 
            --highlightBorder MachineList $
            withDefAttr "list" $
            B.borderWithLabel label $
            hLimit 65 $
            vLimit 20 $
            clickable MachineList $
            --L.renderList listDrawElement (focusedWidgetIs MachineList) l
            L.renderList listDrawElement True (st ^. machineList)

        label = str "Maquina: " <+> cur <+> str " de " <+> total
        cur = case st ^. (machineList . L.listSelectedL) of
                Nothing -> str "-"
                Just i -> str (show (i + 1))

        total = str $ show $ Vec.length $ st ^. (machineList . L.listElementsL)
    in
        mkPage machinePageValidator st $
        vBox [ C.hCenter searchInput
             , C.hCenter toggles
             , C.hCenter mList
             , str " "
             , wid
             ]


------------------------------ UPDATE  - EVENT HANDLERS ---------------------------------

-- | appEventWithSource generate the record field appHandleEvent  :: s -> e -> EventM n (Next s) needed
-- to generate the brick app after supplying a source for the machine list (IO [a])


appEventWithSource :: (String -> IO [String]) -> AppState -> T.BrickEvent WName e -> T.EventM WName (T.Next (AppState))

appEventWithSource source st (T.MouseUp n _ loc) = 
    case n of
      Button pg ->  M.continue (updateDialogButtons st & currentPage .~ pg)
      Toggle t ->  M.continue st
      _ -> M.continue (st & focusedWidget %~ moveFocus n)

appEventWithSource source st (T.MouseDown n _ _ loc) = 
    case n of
      Button pg ->  M.continue (updateDialogButtons st & currentPage .~ pg)
      Toggle t ->  M.continue (toggleSearchColumnAt t st )
      _ -> M.continue (st & focusedWidget %~ moveFocus n)

appEventWithSource source st (T.MouseUp _ _ _) = M.continue $ st

appEventWithSource datasource st (T.VtyEvent e) =
    let 
        fw = F.focusGetCurrent (st ^. focusedWidget)
        selectedMachine = getSelectedMachine st
        --d = st ^. pageDialog
        ds = D.dialogSelection (st ^. pageDialog)
        search = E.getEditContents (st ^. searchTextfield)
    in case e of

        V.EvKey V.KEsc [] -> M.halt defaultState

        -- run the search io action or continue to the confirmation page
        V.EvKey V.KEnter [] -> 
            case fw of
               Just SearchTextfield -> do
                   let st' = st & focusedWidget %~ (moveFocus MachineList)
                   M.suspendAndResume (setMachineList st' <$> datasource (head search))
               _ -> 
                    case (selectedMachine, ds) of 
                        (Just m, Just n) -> M.continue (st & currentPage .~ PageConfirmation)
                        (Nothing, _) -> M.continue st
                        (_, Nothing) -> M.continue st

        V.EvKey V.KLeft [] -> 
            T.handleEventLensed st pageDialog D.handleDialogEvent (V.EvKey V.KLeft []) >>= M.continue

        V.EvKey V.KRight [] -> 
            T.handleEventLensed st pageDialog D.handleDialogEvent (V.EvKey V.KRight []) >>= M.continue

        V.EvKey (V.KChar '\t') [] -> M.continue $ st & focusedWidget %~ F.focusNext

        V.EvKey V.KBackTab [] -> M.continue $ st & focusedWidget %~ F.focusPrev

        V.EvKey k [] ->
            case fw of
               Just SearchTextfield -> T.handleEventLensed st searchTextfield E.handleEditorEvent e >>= M.continue
               Just CommandTextfield -> T.handleEventLensed st commandTextfield E.handleEditorEvent e >>= M.continue
               Just MachineList -> T.handleEventLensed st machineList L.handleListEvent e >>= M.continue
               Just ReadmeVP -> 
                   case k of 
                     V.KDown -> M.vScrollBy (M.viewportScroll ReadmeVP) 1 >> M.continue st
                     V.KUp -> M.vScrollBy (M.viewportScroll ReadmeVP) (-1) >> M.continue st
                     _ -> M.continue st

               Nothing -> M.continue st
                    --T.handleEventLensed st pageDialog D.handleDialogEvent (V.EvKey k []) >>= M.continue

        _ -> M.continue st

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
        defaultDialog = D.dialog (Just "Page Title") Nothing 70
        choices = [ 
                    ("Execute", RunScript)
                  , ("Execute on All", RunScriptOnAll)
                  ]

focusWidgetList = [SearchTextfield, MachineList, CommandTextfield, SideBarOptions, ReadmeVP]

defaultState :: AppState
defaultState = 
    AppState 
    { 
    _currentPage = PageCustom
    , _machineList = L.list MachineList (Vec.fromList []) 1
    , _searchTextfield = (E.editor SearchTextfield (str . unlines) (Just 1) "")
    , _commandTextfield = (E.editor CommandTextfield (str . unlines) (Just 5) "")
    , _focusedWidget = F.focusRing focusWidgetList
    , _pageDialog = defaultActionDialog
    , _infoToggles = [(InfoVPN, True),(InfoHost, False)]
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
    , ("button1" , bg orange)
    , ("button2" , bg V.red)
    , ("button3" , bg V.green)
    , ("separator" ,  V.cyan `on` V.cyan)
    , ("Maquinas", V.red `on` V.black) 
    --, ("list", V.red `on` V.black) 
    --, ("sidebar", V.black `on` V.white) 
    , (B.borderAttr <> "highlighted",  fg V.red)
    , (B.borderAttr <> "main",  fg V.red)
    , (L.listSelectedAttr, V.black `on` V.white)
    , (D.dialogAttr, fg V.white)
    , (D.buttonAttr, V.black `on` V.white)
    , (D.buttonSelectedAttr, bg V.magenta)
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
    let buildVty = do
          v <- V.mkVty =<< V.standardIOConfig
          V.setMode (V.outputIface v) V.Mouse True
          return v
    finalState <-  M.customMain buildVty Nothing (customApp source) defaultState
    handler finalState


--------- HELPERS ----------

listDrawToggles :: (InfoToggle, Bool) -> Widget WName
listDrawToggles (t,b) = 
    case b of
        True -> clickable (Toggle t) $
                (B.border (str " x ")) <+> (str $ show t)

        False -> clickable (Toggle t) $
                (B.border (str "   ")) <+> (str $ show t)

renderHorizontalList f = horizontalJoin . map f
    where horizontalJoin = foldr1 (<+>) . intersperse (str " ")

toggleSearchColumnAt :: InfoToggle -> AppState -> AppState
toggleSearchColumnAt i st = st & (infoToggles . ix m . _2) %~ toggle
    where 
        toggle = not
        m = elemIndex i (map fst (st ^. infoToggles)) ?? 0

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
            Just m -> if m == n && (elem m focusWidgetList) then fr else moveFocus n (F.focusNext fr)

mkButton (name, label, attr) =
			clickable name $
		    withDefAttr attr $
            B.border $
            wid
            where
               wid = T.Widget T.Fixed T.Fixed $ do
							c <- T.getContext
							let h = c^.T.availHeightL
							T.render $ C.hCenter (str label)

-- | Add top right bottom and left padding values to a widget at once
padding t r b l w = padTop (Pad t) $ padBottom (Pad b) $ padLeft (Pad l) $ padRight (Pad r) $ w

updateDialogButtons :: AppState -> AppState
updateDialogButtons st = 
    let
        setDialogButtons' l selected = st & (pageDialog . D.dialogButtonsL) .~ l  & (pageDialog . D.dialogSelectedIndexL) .~ selected
    in
        case st ^. currentPage of
            PageCustom -> setDialogButtons' [("Execute", RunScript),("Execute on All", RunScriptOnAll)] Nothing
            PageConfirmation -> setDialogButtons' [("OK", OK),("Cancel", Cancel)] Nothing
            PageInfo -> setDialogButtons' [("OK", OK),("Cancel", Cancel)] (Just 1)
            _ -> setDialogButtons' [("OK", OK),("Cancel", Cancel), ("Quit", Quit)] (Just 1)

mkPage :: Maybe (AppState -> String) -> AppState -> Widget WName -> Widget WName
mkPage validation st w = 
    let 
        pg = st ^. currentPage
        d = st ^. pageDialog
        style s = C.center $ padBottom (Pad 3) $ s
        w' = maybe w (\v -> style $ str (v st) <=> str " " <=> w) validation
        --w' = maybe w (\v -> str (v st) <=> w) validation
    in
        withDefAttr (fromString $ show pg) $
        C.hCenter $
        D.renderDialog (d & D.dialogTitleL .~ (Just $ show pg)) $
        padAll 1 $
        C.center $ 
        w'

-- Validators

machinePageValidator = Just $ \st ->
    let 
        selectedMachine = getSelectedMachine st
     in case selectedMachine of
          Nothing -> "Por favor busque y seleccione una maquina"
          Just x -> ""
        

------ UTILITIES ---------

(??) = flip fromMaybe

setMachineList ::  AppState -> [String] -> AppState
setMachineList st ls = st &  machineList .~ listMachine where listMachine = L.list MachineList (Vec.fromList  ls) 1

getSelectedMachine :: AppState -> Maybe String
getSelectedMachine st = snd <$> selected 
    where 
        mlist = st ^. machineList
        selected = L.listSelectedElement mlist

aboutText = unlines $ 
            [ " Este programa tiene 3 modalidades de "
            , " funcionamiento que se encuentran en este menu lateral. "
            , " La primera opcion Custom Bash es para ejecutar un script arbitrario "
            , " en la maquina destino."
            , " La opcion Diagnostico deja correr algunos diagnositicos en particular los siguientes"
            , " - Diagnosito 1"
            , " - Diagnosito 2"
            , " - Diagnosito 3"
            ]

dydAscii = ":::::::::  :::   ::: :::::::::  \n:+:    :+: :+:   :+: :+:    :+: \n+:+    +:+  +:+ +:+  +:+    +:+ \n+#+    +:+   +#++:   +#+    +:+ \n+#+    +#+    +#+    +#+    +#+ \n#+#    #+#    #+#    #+#    #+# \n#########     ###    #########  "

