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

greenRainbow :: Double -> (Int32, Int32) -> Color
greenRainbow currentTime (iy, ix) = (r, g, b)
    where x = fromIntegral ix
          y = fromIntegral iy
          t = currentTime * 10
          r = (round ((1 + sin (t +     x * 0.2)) * 127.5))
          g = (round ((1 + sin (t + 4 + x * 0.2)) * 127.5))
          b = (round ((1 + sin (t + 6 + x * 0.2)) * 127.5))

wrongCoolRainbow :: Double -> (Int32, Int32) -> Color
wrongCoolRainbow currentTime (iy, ix) = (r, g, b)
    where x = fromIntegral ix
          y = fromIntegral iy
          t = currentTime * 10
          r = (round ((1 + sin (t +     x * 0.2)) * 127.5))
          g = (round ((1 + sin (t + 2 + x * 0.2)) * 127.5))
          b = (round ((1 + sin (t + 4 + x * 0.2)) * 127.5))

rainbow :: Double -> (Int32, Int32) -> Color
rainbow currentTime (iy, ix) = (r, g, b)
    where x = fromIntegral ix
          y = fromIntegral iy
          t = currentTime * 10
          r = (round (max 0 (sin (t +     x * 0.3) * 200 + 55)))
          g = (round (max 0 (sin (t + 2 + x * 0.3) * 200 + 55)))
          b = (round (max 0 (sin (t + 4 + x * 0.3) * 200 + 55)))


uncalibratedRainbow :: Double -> (Int32, Int32) -> Color
uncalibratedRainbow currentTime (iy, ix) = (r, g, b)
    where x = fromIntegral ix
          y = fromIntegral iy
          t = currentTime * 10
          r = (round (sin (t +     x * 0.3) * 127 + 128))
          g = (round (sin (t + 2 + x * 0.3) * 127 + 128))
          b = (round (sin (t + 4 + x * 0.3) * 127 + 128))

variedRainbow :: Double -> (Int32, Int32) -> Color
variedRainbow currentTime (iy, ix) = (r, g, b)
    where x = fromIntegral ix
          y = fromIntegral iy
          t = currentTime * 10
          r = (round (sin (t +       x * 0.3) * 127 + 128))
          g = (round (sin (t + 2 + x * 0.33) * 127 + 128))
          b = (round (sin (t + 4 + x * 0.37) * 127 + 128))

oneColor :: Color -> Double -> (Int32, Int32) -> Color
oneColor (cr, cg, cb) currentTime (iy, ix) = (r, g, b)
    where x = fromIntegral ix
          y = fromIntegral iy
          t = currentTime * 10
          r = (min cr $ round (sin (t + x / 2) * 127 + 128))
          g = (min cg $ round (sin (t + x / 2) * 127 + 128))
          b = (min cb $ round (sin (t + x / 2) * 127 + 128))