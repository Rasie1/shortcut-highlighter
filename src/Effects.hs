module Effects where

import Color
import Data.Int

gradients1 :: Double -> (Int32, Int32) -> Color
gradients1 currentTime (iy, ix) = (r, g, b)
    where x = fromIntegral ix
          y = fromIntegral iy
          t = currentTime * 500
          r = (round (x * 34  + t))
          g = (round (x * 33  + t))
          b = (round (y * 16  + t))