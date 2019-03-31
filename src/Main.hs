import Color
import Data.Int
import Control.Monad
import Devices.Razer
import DBus.Client
import Control.Concurrent.Thread.Delay
import Data.Time.Clock
import Data.Time.Clock.POSIX

shifted :: NominalDiffTime -> (Int32, Int32) -> Color
shifted it (iy, ix) = (r, g, b)
    where x = fromIntegral ix
          y = fromIntegral iy
          t = it * 100
          r = (round (x * 34  + t))
          g = (round (x * 33  + t))
          b = (round (y * 16  + t))

main = do
    client <- connectSession
    dimensions <- getMatrixDimensions client
    forever $ do
        t <- getPOSIXTime
        let f = fillKeyboard (shifted t) dimensions
        print (shifted t (10, 16))
        delay 40000
        setFrame f client