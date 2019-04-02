import Data.Int
import Data.Maybe
import Control.Monad
import DBus.Client
import Control.Concurrent.Thread.Delay
import Data.Time.Clock
import Data.Time.Clock.POSIX

import System.Posix
import System.IO
import Data.Bits

import Devices.Razer
import Devices.System
import Effects
import Color
import Keyboard


keyboardUpdateDelay = 40000
frequentInfoUpdateRate = 0.4
infrequentInfoUpdateRate = 60.0

light :: Bool -> (Int32, Int32) -> KeyboardLightingState -> IO (Maybe Frame)
light new dim KeyboardLightingState {_mode = mode, _time = t} = 
    return $ case mode of
        LightingDefault -> Just $ fillKeyboard (oneColor colorRed t) dim
        _               -> Just $ fillKeyboard (oneColor colorBlue t) dim


step previousUpdateTime previousTime client dimensions cpu keyboardDaemonFile state = do
    t <- getPOSIXTime

    let shouldUpdateInfo = t - previousUpdateTime > frequentInfoUpdateRate
    cpu <- if shouldUpdateInfo 
                then updateCPUUsage (snd cpu)
                else return cpu

    let deltaTime = t - previousTime

    ready <- hReady keyboardDaemonFile
    maybeSignal <- if ready 
                      then do char <- hGetChar keyboardDaemonFile
                              return . Just . charToSignal $ char
                      else return Nothing
    let newState = state { _time = _time state + realToFrac deltaTime * fst cpu
                         , _mode = fromMaybe (_mode state) $ maybeSignal >>= signalToMode }
    nextFrame <- light (isJust maybeSignal) dimensions newState

    -- print (variedRainbow newEffectsTime (1, 1))
    when (isJust nextFrame) $ setFrame (fromJust nextFrame) client

    delay keyboardUpdateDelay

    let newTime = if shouldUpdateInfo then t else previousUpdateTime
    step newTime t client dimensions cpu keyboardDaemonFile newState


getKeyboardDaemonFile path = do
    exists <- fileExist path
    if not exists
        then createNamedPipe path (ownerReadMode .|. ownerWriteMode .|. namedPipeMode)
        else return ()
    openFile path ReadMode


main = do
    client <- connectSession
    dimensions <- getMatrixDimensions client
    startTime <- getPOSIXTime
    cpu <- getCPUUsage

    let filePath = "/etc/rasiel/keyboardrasiel2"
    -- let filePath = "/home/rasiel/.config/i3/keyboardrasiel"
    keyboardDaemonFile <- getKeyboardDaemonFile filePath
    let state = KeyboardLightingState { _mode = LightingDefault, _time = 0.0 }

    step startTime startTime client dimensions cpu keyboardDaemonFile state