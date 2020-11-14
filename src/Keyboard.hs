module Keyboard where

import Data.Int
import Data.Char (ord)
import Control.Monad.Trans.State
import Data.Functor.Identity

import Color
import Effects
import Layouts
import Devices.I3 (WorkspaceConfig(..))
import SystemState

light :: Bool -> (Int32, Int32) -> SystemState -> Maybe Frame
light new dim
      SystemState { _keyboard = KeyboardLightingState { _mode = mode
                                                      , _time = t
                                                      , _workspaces = workspaces
                                                      }
                  , _language = lang} = 
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
        _               -> fromFrame (lightWorkspaces workspaces)
    where keyboardWithEffect :: Frame
          keyboardWithEffect = fillKeyboard effect dim
          effect = case lang of
                        LangUS -> rainbow t
                        LangRU -> wrongCoolRainbow t --disassemblingRainbow t 
          fromFrame actions = Just . fst . snd $ runState actions (keyboardWithEffect, colorBlack)
          withNewFrame :: StateT (Frame, Color) Identity () -> Maybe Frame
          withNewFrame actions = 
                if not new 
                    then Nothing 
                    else Just . fst . snd $ (runState actions (solidColor colorBlack dim, languageToColor lang)) 

lightWorkspaces :: [WorkspaceConfig] -> StateT (Frame, Color) Identity ()
lightWorkspaces ws = mapM_ lightWorkspace (zip [1..15] ws)

lightWorkspace :: (Int32, WorkspaceConfig) -> StateT (Frame, Color) Identity ()
lightWorkspace (i, ws) = setColor (1, i) color
    where color = case ws of
                WorkspaceEmpty  -> colorDarkgreen
                WorkspaceWindow -> colorYellow
                WorkspaceUrgent -> colorOrange
                WorkspaceActive -> colorWhite

languageToColor LangRU = colorRed
languageToColor LangUS = colorBlue

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
    deriving (Show, Eq)

data KeyboardSignal =
      Modifier ModifierSignal
    | SignalDefault
    | SignalSwitchlang
    | SignalUpdateWorkspaces
    | SignalSetReactive
    | SignalUnsetReactive
    | SignalIncreaseBulbBrightness
    | SignalDecreaseBulbBrightness
    deriving (Show, Eq)

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
    100 -> SignalIncreaseBulbBrightness
    101 -> SignalDecreaseBulbBrightness