module SystemState where

import Data.Int
import Devices.I3 (WorkspaceConfig(..))

data SystemState = SystemState 
                   { _keyboard :: KeyboardLightingState
                   , _language :: Language
                   , _bulbBrightness :: Int16 
                   }

data KeyboardLightingMode = LightingDefault
                          | LightingCtrlShiftSuper
                          | LightingCtrlSuper
                          | LightingCtrlAltShift
                          | LightingCtrlShift
                          | LightingCtrlAlt
                          | LightingCtrl
                          | LightingShiftSuper
                          | LightingAltSuper
                          | LightingSuper
                          | LightingAltShift
                          | LightingShift
                          | LightingAlt

data Language = LangUS | LangRU
data KeyboardLightingState = 
    KeyboardLightingState { _mode :: KeyboardLightingMode
                          , _time :: Double
                          , _workspaces :: [WorkspaceConfig] 
                          , _previousBrightness :: Double
                          }