module Keyboard where

import Data.Int
import Data.Char (ord)
import Control.Monad.State.Lazy

import Color
import Effects
import Layouts


light :: Bool -> (Int32, Int32) -> KeyboardLightingState -> Maybe Frame
light new dim KeyboardLightingState {_mode = mode, _time = t} = 
    case mode of
        LightingCtrlShiftSuper -> withNewFrame lightCtrlShiftSuper
        LightingCtrlSuper -> withNewFrame lightCtrlSuper
        LightingCtrlAltShift -> withNewFrame lightCtrlAltShift
        LightingCtrlShift -> withNewFrame lightCtrlShift
        LightingCtrlAlt -> withNewFrame lightCtrlAlt
        LightingCtrl -> withNewFrame lightCtrl
        LightingShiftSuper -> withNewFrame lightShiftSuper
        LightingAltSuper -> withNewFrame lightAltSuper
        LightingSuper -> withNewFrame lightSuper
        LightingAltShift -> withNewFrame lightAltShift
        LightingShift -> withNewFrame lightShift
        LightingAlt -> withNewFrame lightAlt
        _               -> Just $ fillKeyboard (rainbow t) dim
    where withNewFrame :: State Frame () -> Maybe Frame
          withNewFrame actions = if not new then Nothing else Just . snd $ (runState actions (fillKeyboard (oneColor colorRed t) dim)) 


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

data KeyboardLightingState = KeyboardLightingState { _mode :: KeyboardLightingMode, _time :: Double }

signalToMode :: KeyboardSignal -> Maybe KeyboardLightingMode
signalToMode SignalCtrlShiftSuper = Just LightingCtrlShiftSuper
signalToMode SignalCtrlSuper = Just LightingCtrlSuper
signalToMode SignalCtrlAltShift = Just LightingCtrlAltShift
signalToMode SignalCtrlShift = Just LightingCtrlShift
signalToMode SignalCtrlAlt = Just LightingCtrlAlt
signalToMode SignalCtrl = Just LightingCtrl
signalToMode SignalShiftSuper = Just LightingShiftSuper
signalToMode SignalAltSuper = Just LightingAltSuper
signalToMode SignalSuper = Just LightingSuper
signalToMode SignalAltShift = Just LightingAltShift
signalToMode SignalShift = Just LightingShift
signalToMode SignalAlt = Just LightingAlt
signalToMode SignalDefault = Just LightingDefault
signalToMode _ = Nothing

data KeyboardSignal =
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
    | SignalDefault
    | SignalSwitchlang
    | SignalUpdateWorkspaces
    | SignalSetReactive
    | SignalUnsetReactive
    deriving (Show)

charToSignal :: Char -> KeyboardSignal
charToSignal c = case ord c of
    0  -> SignalCtrlShiftSuper
    1  -> SignalCtrlSuper
    2  -> SignalCtrlAltShift
    3  -> SignalCtrlShift
    4  -> SignalCtrlAlt
    5  -> SignalCtrl
    6  -> SignalShiftSuper
    7  -> SignalAltSuper
    8  -> SignalSuper
    9  -> SignalAltShift
    10 -> SignalShift
    11 -> SignalAlt
    12 -> SignalDefault
    15 -> SignalSwitchlang
    14 -> SignalUpdateWorkspaces
    56 -> SignalSetReactive
    57 -> SignalUnsetReactive