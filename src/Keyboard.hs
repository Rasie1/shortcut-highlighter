module Keyboard where

import Data.Int
import Data.Char (ord)
import Control.Monad.State.Lazy

import Color
import Effects
import Layouts
import Devices.I3 ( WorkspaceConfig(..))

light :: Bool -> (Int32, Int32) -> KeyboardLightingState -> Maybe Frame
light new dim KeyboardLightingState {_mode = mode, _time = t, _workspaces = workspaces, _language = lang} = 
    case mode of
        LightingCtrlShiftSuper -> withNewFrame lightCtrlShiftSuper
        LightingCtrlSuper -> withNewFrame lightCtrlSuper
        LightingCtrlAltShift -> withNewFrame lightCtrlAltShift
        LightingCtrlShift -> withNewFrame lightCtrlShift
        LightingCtrlAlt -> withNewFrame lightCtrlAlt
        LightingCtrl -> withNewFrame lightCtrl
        LightingShiftSuper -> withNewFrame (lightShiftSuper >> lightWorkspaces workspaces)
        LightingAltSuper -> withNewFrame lightAltSuper
        LightingSuper -> withNewFrame (lightSuper >> lightWorkspaces workspaces)
        LightingAltShift -> withNewFrame lightAltShift
        LightingShift -> withNewFrame lightShift
        LightingAlt -> withNewFrame lightAlt
        _               -> Just $ fillKeyboard effect dim
    where effect = case lang of
                        LangUS -> rainbow t
                        LangRU -> wrongCoolRainbow t
          withNewFrame :: State (Frame, Color) () -> Maybe Frame
          withNewFrame actions = 
                if not new 
                    then Nothing 
                    else Just . fst . snd $ (runState actions (solidColor colorBlack dim, languageToColor lang)) 

lightWorkspaces ws = mapM_ lightWorkspace (zip [1..15] ws)
lightWorkspace (i, ws) = setColor (1, i) color
    where color = case ws of
                WorkspaceEmpty  -> colorDarkgreen
                WorkspaceWindow -> colorYellow
                WorkspaceUrgent -> colorOrange
                WorkspaceActive -> colorWhite

languageToColor LangRU = colorRed
languageToColor LangUS = colorBlue

data KeyboardLightingMode = LightingDefault
                          | LightingCtrlShiftSuper
                          | LightingCtrlSuper
                          | LightingCtrlAltShift
                          | LightingCtrlShift
                          | LightingCtrlAlt
                          | LightingCtrl
                          | LightingShiftSuper
                          | LightingAltSuper
                          | LightingSuper
                          | LightingAltShift
                          | LightingShift
                          | LightingAlt

data Language = LangUS | LangRU
data KeyboardLightingState = 
    KeyboardLightingState { _mode :: KeyboardLightingMode
                          , _time :: Double
                          , _language :: Language
                          , _workspaces :: [WorkspaceConfig] 
                          , _previousBrightness :: Double
                          }

signalToMode :: KeyboardSignal -> Maybe KeyboardLightingMode
signalToMode (Modifier SignalCtrlShiftSuper) = Just LightingCtrlShiftSuper
signalToMode (Modifier SignalCtrlSuper) = Just LightingCtrlSuper
signalToMode (Modifier SignalCtrlAltShift) = Just LightingCtrlAltShift
signalToMode (Modifier SignalCtrlShift) = Just LightingCtrlShift
signalToMode (Modifier SignalCtrlAlt) = Just LightingCtrlAlt
signalToMode (Modifier SignalCtrl) = Just LightingCtrl
signalToMode (Modifier SignalShiftSuper) = Just LightingShiftSuper
signalToMode (Modifier SignalAltSuper) = Just LightingAltSuper
signalToMode (Modifier SignalSuper) = Just LightingSuper
signalToMode (Modifier SignalAltShift) = Just LightingAltShift
signalToMode (Modifier SignalShift) = Just LightingShift
signalToMode (Modifier SignalAlt) = Just LightingAlt
signalToMode SignalDefault = Just LightingDefault
signalToMode _ = Nothing

data ModifierSignal = 
      SignalCtrlShiftSuper
    | SignalCtrlSuper
    | SignalCtrlAltShift
    | SignalCtrlShift
    | SignalCtrlAlt
    | SignalCtrl
    | SignalShiftSuper
    | SignalAltSuper
    | SignalSuper
    | SignalAltShift
    | SignalShift
    | SignalAlt
    deriving (Show)

data KeyboardSignal =
      Modifier ModifierSignal
    | SignalDefault
    | SignalSwitchlang
    | SignalUpdateWorkspaces
    | SignalSetReactive
    | SignalUnsetReactive
    deriving (Show)

charToSignal :: Char -> KeyboardSignal
charToSignal c = case ord c of
    0  -> Modifier SignalCtrlShiftSuper
    1  -> Modifier SignalCtrlSuper
    2  -> Modifier SignalCtrlAltShift
    3  -> Modifier SignalCtrlShift
    4  -> Modifier SignalCtrlAlt
    5  -> Modifier SignalCtrl
    6  -> Modifier SignalShiftSuper
    7  -> Modifier SignalAltSuper
    8  -> Modifier SignalSuper
    9  -> Modifier SignalAltShift
    10 -> Modifier SignalShift
    11 -> Modifier SignalAlt
    12 -> SignalDefault
    15 -> SignalSwitchlang
    14 -> SignalUpdateWorkspaces
    56 -> SignalSetReactive
    57 -> SignalUnsetReactive