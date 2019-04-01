import Color
import Data.Int
import Control.Monad
import Devices.Razer
import Devices.System
import DBus.Client
import Control.Concurrent.Thread.Delay
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Effects

keyboardUpdateRate = 40000
frequentInfoUpdateRate = 1.0
infrequentInfoUpdateRate = 60.0

step previousUpdateTime previousTime client dimensions cpu effectsData = do
    t <- getPOSIXTime

    let shouldUpdateInfo = t - previousUpdateTime > frequentInfoUpdateRate
    cpu <- if shouldUpdateInfo 
                then updateCPUUsage (snd cpu)
                else return cpu

    let deltaTime = t - previousTime
    let newEffectsData = effectsData + realToFrac deltaTime * fst cpu
    let frame = fillKeyboard (gradients1 newEffectsData) dimensions

    setFrame frame client

    delay keyboardUpdateRate

    let newTime = if shouldUpdateInfo then t else previousUpdateTime
    step newTime t client dimensions cpu newEffectsData

main = do
    client <- connectSession
    dimensions <- getMatrixDimensions client
    startTime <- getPOSIXTime
    cpu <- getCPUUsage

    step startTime startTime client dimensions cpu 0