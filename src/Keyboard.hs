module Keyboard where

import Data.Int
import Data.Char (ord)
import Color
import Control.Monad.State.Lazy

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

setColor :: (Int32, Int32) -> Color -> State Frame ()
setColor pos c = return ()

lightModifiers :: State Frame ()
lightModifiers = do
    setLeftShift colorRed

-- Layout for German keyboard. Sorry, you'll have to remap keys for your keyboard.

setEscape = setColor (0,1)
setF1 = setColor (0,1)
setF2 = setColor (0,1)
setF3 = setColor (0,1)
setF4 = setColor (0,1)
setF5 = setColor (0,1)
setF6 = setColor (0,1)
setF7 = setColor (0,1)
setF8 = setColor (0,1)
setF9 = setColor (0,1)
setF10 = setColor (0,1)
setF11 = setColor (0,1)
setF12 = setColor (0,1)
setInsert = setColor (0,1)
setDelete = setColor (0,1)

setCircumflex = setColor (1,1)
setDigit0 = setColor (1,1)
setDigit1 = setColor (1,1)
setDigit2 = setColor (1,1)
setDigit3 = setColor (1,1)
setDigit4 = setColor (1,1)
setDigit5 = setColor (1,1)
setDigit6 = setColor (1,1)
setDigit7 = setColor (1,1)
setDigit8 = setColor (1,1)
setDigit9 = setColor (1,1)
setEszett = setColor (1,1)
setBackquote = setColor (1,1)
setBackspace = setColor (1,1)

setTab = setColor (2,1)
setQ = setColor (2,1)
setW = setColor (2,1)
setE = setColor (2,1)
setR = setColor (2,1)
setT = setColor (2,1)
setZ = setColor (2,1)
setU = setColor (2,1)
setI = setColor (2,1)
setO = setColor (2,1)
setP = setColor (2,1)
setUumlaut = setColor (2,1)
setPlus = setColor (2,1)
setEnter = setColor (2,1)

setCapsLock = setColor (3,1)
setA = setColor (3,1)
setS = setColor (3,1)
setD = setColor (3,1)
setF = setColor (3,1)
setG = setColor (3,1)
setH = setColor (3,1)
setJ = setColor (3,1)
setK = setColor (3,1)
setL = setColor (3,1)
setOumlaut = setColor (3,1)
setAumlaut = setColor (3,1)
setHash = setColor (3,1)

setLeftShift = setColor (4,1)
setLess = setColor (4,1)
setY = setColor (4,1)
setX = setColor (4,1)
setC = setColor (4,1)
setV = setColor (4,1)
setB = setColor (4,1)
setN = setColor (4,1)
setM = setColor (4,1)
setComma = setColor (4,1)
setPeriod = setColor (4,1)
setMinus = setColor (4,1)
setRightShift = setColor (4,1)

setLeftControl = setColor (5,1)
setLeftFn = setColor (5,2)
setLeftSuper = setColor (5,3)
setLeftAlt = setColor (5,4)
setRightAlt = setColor (5,9)
setRightFn = setColor (5,10)
setRightControl = setColor (5,11)
setArrowLeft = setColor (5,12)
setArrowUp = setColor (5,13)
setArrowRight = setColor (5,14)
setArrowDown = setColor (5,15)

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