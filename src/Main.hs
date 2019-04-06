import Data.Int
import Data.Maybe
import Data.List
import Control.Monad
import qualified DBus.Client as DBus
import Control.Concurrent.Thread.Delay
import Data.Time.Clock
import Data.Time.Clock.POSIX

import System.Posix
import System.IO
import Data.Bits

import Devices.Razer
import Devices.System
import qualified Devices.I3 as I3
import Effects
import Color
import Keyboard

keyboardUpdateDelay = 40000
frequentInfoUpdateRate = 0.4
infrequentInfoUpdateRate = 2.0

step previousUpdateTime previousTime dbus i3 dimensions cpu keyboardDaemonFile state = do
    t <- getPOSIXTime

    let shouldUpdateInfo = t - previousUpdateTime > frequentInfoUpdateRate
    cpu <- if shouldUpdateInfo 
                then updateCPUUsage (snd cpu)
                else return cpu

    let deltaTime = t - previousTime

    inputSignals <- readAvailableInput keyboardDaemonFile []
    let maybeSignal = getLatestModeSignal inputSignals
                        
    newLanguage <- if shouldSwitchLanguage inputSignals
                        then case _language state of
                            LangUS -> setLanguageRu >> return LangRU
                            LangRU -> setLanguageUs >> return LangUS
                        else return $ _language state

    prevBrightness <- case maybeSignal of
                            Just (Modifier _) -> do brightness <- getKeyboardBrightness dbus
                                                    if (brightness < 99.9) 
                                                        then do setKeyboardBrightness 100.0 dbus
                                                                return brightness
                                                        else return $ _previousBrightness state
                            Just SignalDefault -> case _mode state of
                                                    LightingDefault -> return $ _previousBrightness state
                                                    _ -> do setKeyboardBrightness (_previousBrightness state) dbus
                                                            return $ _previousBrightness state
                            _ -> return $ _previousBrightness state

    newWorkspaces <- if shouldUpdateWorkspaces inputSignals 
                        then I3.getWorkspacesConfig i3
                        else return $ _workspaces state

    let newState = 
            state { _time = _time state + realToFrac deltaTime * fst cpu
                  , _mode = fromMaybe (_mode state) $ maybeSignal >>= signalToMode
                  , _workspaces = newWorkspaces 
                  , _previousBrightness = prevBrightness
                  , _language = newLanguage }
    let nextFrame = light (isJust maybeSignal || shouldUpdateWorkspaces inputSignals) 
                          dimensions newState

    when (isJust nextFrame) $ setFrame (fromJust nextFrame) dbus
    delay keyboardUpdateDelay

    let newTime = if shouldUpdateInfo then t else previousUpdateTime
    step newTime t dbus i3 dimensions cpu keyboardDaemonFile newState

shouldUpdateWorkspaces :: [KeyboardSignal] -> Bool
shouldUpdateWorkspaces = elem SignalUpdateWorkspaces

shouldSwitchLanguage :: [KeyboardSignal] -> Bool
shouldSwitchLanguage = elem SignalSwitchlang

getLatestModeSignal :: [KeyboardSignal] -> Maybe KeyboardSignal
getLatestModeSignal (x:xs) = case x of 
                                Modifier a -> Just (Modifier a)
                                SignalDefault -> Just SignalDefault
                                _ -> getLatestModeSignal xs

getLatestModeSignal [] = Nothing

getKeyboardDaemonFile path = do
    exists <- fileExist path
    if not exists
        then createNamedPipe path (ownerReadMode .|. ownerWriteMode .|. namedPipeMode)
        else return ()
    openFile path ReadMode

readAvailableInput f xs = do 
    ready <- hReady f
    if ready then do char <- hGetChar f
                     readAvailableInput f ((charToSignal char):xs)
             else return xs

main = do
    dbus <- DBus.connectSession
    dimensions <- getMatrixDimensions dbus
    brightness <- getKeyboardBrightness dbus

    startTime <- getPOSIXTime
    cpu <- getCPUUsage

    i3 <- I3.connectI3
    ws <- I3.getWorkspacesConfig i3

    let filePath = "/etc/rasiel/keyboardrasiel"
    keyboardDaemonFile <- getKeyboardDaemonFile filePath
    setLanguageUs
    let state = KeyboardLightingState { _mode = LightingDefault
                                      , _time = 0.0
                                      , _workspaces = ws
                                      , _language = LangUS 
                                      , _previousBrightness = brightness}

    step startTime startTime dbus i3 dimensions cpu keyboardDaemonFile state