module Color where

import Prelude
import Data.Int
import Data.Word

type ColorComponent = Word8

type Color = (ColorComponent, ColorComponent, ColorComponent)

type ColorFunction = (Int32, Int32) -> Color
type TimeColorFunction = Double -> ColorFunction


type Brightness = Word8

data Frame = Frame { _dimensions :: (Word8, Word8), _keys :: [[Color]] }

colorRed :: Color
colorRed = (255, 0, 0)

colorGreenplus :: Color
colorGreenplus = (60, 255, 20)

colorGreen :: Color
colorGreen = (0, 255, 0)

colorLightgreen :: Color
colorLightgreen = (55, 255, 55)

colorDarkgreen :: Color
colorDarkgreen = (0, 10, 0)

colorBlue :: Color
colorBlue = (0, 0, 255)

colorCyan :: Color
colorCyan = (0, 255, 255)

colorMagenta :: Color
colorMagenta = (255, 0, 255)

colorYellow :: Color
colorYellow = (255, 255, 0)

colorBlack :: Color
colorBlack = (0, 0, 0)

colorWhite :: Color
colorWhite = (255, 255, 255)

colorOrange :: Color
colorOrange = (255, 64, 0)

fillKeyboard :: ColorFunction -> (Int32, Int32) -> Frame
fillKeyboard color dim = Frame { _dimensions = (fromIntegral h, fromIntegral w), _keys = keys }
    where (h, w) = dim
          indices :: [[(Int32, Int32)]]
          indices = [zip (replicate (fromIntegral w) i) [1..w] | i <- [1..h]]
          keys :: [[Color]]
          keys = map (map color) indices

solidColor :: (Int32, Int32) -> Color -> Frame
solidColor dim color = Frame { _dimensions = (fromIntegral h, fromIntegral w), _keys = keys }
    where (h, w) = dim
          keys = replicate (fromIntegral h) (replicate (fromIntegral w) color)