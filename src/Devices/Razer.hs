{-# LANGUAGE OverloadedStrings #-}

module Devices.Razer 
    ( setWaveRight
    , setWaveLeft
    , setFrame
    , getMatrixDimensions
    , setKeyboardBrightness
    , getKeyboardBrightness
    ) where 

import Data.List (sort)
import Data.Int
import Data.Maybe
import Data.Binary as Binary
import Data.Binary.Put as Put
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as L8
import DBus
import DBus.Client
import Color

deviceAddress = "/org/razer/device/BY1750A44000357"
miscMethod    = "razer.device.misc"

-- Need to do it multiple times because of hardware bug
getKeyboardBrightnessOnce :: Client -> IO Double
getKeyboardBrightnessOnce c = do
    reply <- call_ c (methodCall deviceAddress "razer.device.lighting.brightness" "getBrightness") 
                    { methodCallDestination = Just "org.razer" }
    let x:_ = (map (fromJust . fromVariant) (methodReturnBody reply)) :: [Double]
    return x

getKeyboardBrightness :: Client -> IO Double
getKeyboardBrightness c = do
    a <- getKeyboardBrightnessOnce c
    b <- getKeyboardBrightnessOnce c
    c <- getKeyboardBrightnessOnce c
    return (max a (max b c))

setKeyboardBrightness :: Double -> Client -> IO ()
setKeyboardBrightness brightness c = do
    call_ c (methodCall deviceAddress "razer.device.lighting.brightness" "setBrightness") 
            { methodCallDestination = Just "org.razer"
            , methodCallBody = [toVariant brightness] }
    return ()

setWaveRight :: Client -> IO ()
setWaveRight c = do
    call_ c (methodCall deviceAddress "razer.device.lighting.chroma"  "setWave") 
            { methodCallDestination = Just "org.razer"
            , methodCallBody = [toVariant (1 :: Int32)] }
    return ()

setWaveLeft :: Client -> IO ()
setWaveLeft c = do
    call_ c (methodCall deviceAddress "razer.device.lighting.chroma"  "setWave") 
            { methodCallDestination = Just "org.razer"
            , methodCallBody = [toVariant (2 :: Int32)] }
    return ()

setCustom :: Client -> IO ()
setCustom c = do
    call_ c (methodCall deviceAddress "razer.device.lighting.chroma"  "setCustom") 
            { methodCallDestination = Just "org.razer" }
    return ()

sendCustom :: Frame -> Client -> IO ()
sendCustom frame c = do
    call_ c (methodCall deviceAddress "razer.device.lighting.chroma"  "setKeyRow") 
            { methodCallDestination = Just "org.razer" 
            , methodCallBody = [toVariant (encodeFrame frame)] }
    return ()

setFrame :: Frame -> Client -> IO ()
setFrame f c = sendCustom f c >> setCustom c

getMatrixDimensions :: Client -> IO (Int32, Int32)
getMatrixDimensions c = do
    reply <-call_ c (methodCall deviceAddress "razer.device.misc"  "getMatrixDimensions") 
                    { methodCallDestination = Just "org.razer" }
    let [x:y:[]] = (map (fromJust . fromVariant) (methodReturnBody reply)) :: [[Int32]]
    return (x, y)


encodeFrame :: Frame -> L8.ByteString
encodeFrame frame = Put.runPut (mapM_ putRow rowInfo)
        where (h, w) = _dimensions frame
              rowLastIndex = h - 1
              rowInfo :: [(Word8, (Word8, [Color]))]
              rowInfo = zip (replicate (fromIntegral h) (w - 1)) . zip [0..rowLastIndex] $ _keys frame

putRow :: (Word8, (Word8, [Color])) -> Put
putRow (rowEnd, (rowIndex, keys)) = index >> start >> end >> row
    where index = putWord8 rowIndex
          start = putWord8 0
          end   = putWord8 rowEnd
          row   = mapM_ putColor keys

putColor :: Color -> Put
putColor (r, g, b) = putWord8 r >> putWord8 g >> putWord8 b