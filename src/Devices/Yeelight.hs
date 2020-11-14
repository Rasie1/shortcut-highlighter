{-# LANGUAGE OverloadedStrings #-}
module Devices.Yeelight where

import System.IO (hPutStr, hClose)
import System.Process.Typed
import Data.Int
import qualified Data.ByteString.Lazy.Char8 as L8

type Bulb = String

bulbs :: [Bulb]
bulbs = [ "/home/rasiel/Projects/yeelight-shell-scripts/yeelight-brightness.sh 0 "
        , "/home/rasiel/Projects/yeelight-shell-scripts/2/yeelight-brightness.sh 0 " ]


setBrightness :: Int16 -> Bulb -> IO ()
setBrightness brightness bulb = do
    let bulbCommand = shell . mappend bulb . show $ brightness
    (_, _, err) <- readProcess bulbCommand
    -- print err
    return ()

brightnessStep = 10

increaseBrightness :: Int16 -> IO Int16
increaseBrightness prev = do
    let newBrightness = min 100 (max 1 (prev + brightnessStep))
    print newBrightness
    mapM_ (setBrightness newBrightness) bulbs
    return newBrightness


decreaseBrightness :: Int16 -> IO Int16
decreaseBrightness prev = do
    let newBrightness = min 100 (max 1 (prev - brightnessStep))
    print newBrightness
    mapM_ (setBrightness newBrightness) bulbs
    return newBrightness

