{-# LANGUAGE OverloadedStrings #-}

module Devices.System (getCPUUsage
                      , getCPUUsageData
                      , updateCPUUsage
                      , setLanguageRu
                      , setLanguageUs) where

import System.IO (hPutStr, hClose)
import System.Process.Typed
import Data.Int
import qualified Data.ByteString.Lazy.Char8 as L8

cpuDataCommand = "grep -w cpu /proc/stat"
setLanguageRuCommand = "setxkbmap ru,us"
setLanguageUsCommand = "setxkbmap us,ru"

type CPUUsageData = (Int32, Int32, Int32)

getCPUUsage :: IO (Double, CPUUsageData)
getCPUUsage = do
    (out, err) <- readProcess_ cpuDataCommand
    let unpacked = L8.unpack out
    let s = take (length unpacked - 1) unpacked
    let _:as:_:bs:cs:_ = words s
    let a = read as
    let b = read bs
    let c = read cs
    let ret = fromIntegral (a + b) / fromIntegral (a + b + c)
    return (ret, (a, b, c))

getCPUUsageData :: IO CPUUsageData
getCPUUsageData = do
    (out, err) <- readProcess_ cpuDataCommand
    let unpacked = L8.unpack out
    let s = take (length unpacked - 1) unpacked
    let _:a:_:b:c:_ = words s
    return (read a, read b, read c)

updateCPUUsage :: CPUUsageData -> IO (Double, CPUUsageData)
updateCPUUsage (oldA, oldB, oldC) = do
    (a, b, c) <- getCPUUsageData
    let ret = fromIntegral (oldA + oldB - a - b) / fromIntegral (oldA + oldB + oldC - a - b - c)
    return (ret, (a, b, c))

setLanguageRu :: IO ()
setLanguageRu = do
    readProcess_ setLanguageRuCommand 
    return ()
setLanguageUs :: IO ()
setLanguageUs = do
    readProcess_ setLanguageUsCommand 
    return ()