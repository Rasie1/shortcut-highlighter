module Layouts where
import Color
import Control.Monad.State.Lazy

lightModifiers = do
    (_, c) <- get
    setLeftShift c
    setLeftControl c
    setLeftFn c
    setLeftSuper c
    setLeftAlt c
    setRightShift c
    setRightControl c
    setRightFn c
    setRightAlt c

lightAlt = do
    lightModifiers
    setLeftAlt colorWhite
    setRightAlt colorWhite
    setArrowLeft colorGreen
    setArrowRight colorGreen
    setTab colorGreen
    setBackspace colorGreen
    setE colorGreen
    setO colorGreen

lightCtrlShiftSuper = do
    lightModifiers
    setLeftControl colorWhite
    setLeftShift colorWhite
    setLeftSuper colorWhite
    setRightControl colorWhite
    setRightShift colorWhite

lightCtrlSuper = do
    lightModifiers
    setLeftControl colorWhite
    setLeftSuper colorWhite
    setRightControl colorWhite

lightCtrlAltShift = do
    lightModifiers
    setLeftControl colorWhite
    setLeftShift colorWhite
    setLeftAlt colorWhite
    setRightControl colorWhite
    setRightShift colorWhite
    setRightAlt colorWhite

lightCtrlShift = do
    lightModifiers
    setLeftControl colorWhite
    setLeftShift colorWhite
    setRightControl colorWhite
    setRightShift colorWhite
    set1 colorGreen
    set2 colorGreen
    set3 colorGreen
    set4 colorGreen
    set5 colorGreen
    set6 colorGreen
    set7 colorGreen
    set8 colorGreen
    set9 colorGreen
    setEszett colorGreen
    setBackquote colorGreen
    setTab colorGreen
    setW colorGreen
    setE colorCyan
    setR colorCyan
    setT colorYellow
    setP colorGreen
    setS colorGreen
    setF colorYellow
    setG colorGreen
    setY colorGreen
    setX colorCyan
    setC colorYellow
    setV colorYellow
    setComma colorGreen
    setPeriod colorGreen
    setMinus colorGreen
    setArrowLeft colorGreen
    setArrowUp colorGreen
    setArrowDown colorGreen
    setArrowRight colorGreen

lightCtrlAlt = do
    lightModifiers
    setLeftControl colorWhite
    setLeftAlt colorWhite
    setRightControl colorWhite
    setRightAlt colorWhite
    setArrowLeft colorGreen
    setArrowUp colorGreen
    setArrowDown colorGreen
    setArrowRight colorGreen

lightCtrl = do
    lightModifiers
    setLeftControl colorWhite
    setRightControl colorWhite
    set1 colorGreen
    set2 colorGreen
    set3 colorGreen
    set4 colorGreen
    set5 colorGreen
    set6 colorGreen
    set7 colorGreen
    set8 colorGreen
    set9 colorGreen
    set0 colorGreen
    setEszett colorGreen
    setBackquote colorGreen
    setTab colorGreen
    setQ colorRed
    setW colorGreen
    setR colorGreen
    setT colorYellow
    setO colorGreen
    setP colorGreen
    setUumlaut colorCyan
    setPlus colorCyan
    setEnter colorGreen
    setA colorYellow
    setS colorMagenta
    setD colorGreen
    setF colorYellow
    setG colorGreen
    setJ colorGreen
    setK colorGreen
    setL colorGreen
    setOumlaut colorCyan
    setAumlaut colorCyan
    setY colorYellow
    setX colorYellow
    setC colorYellow
    setV colorYellow
    setComma colorGreen
    setPeriod colorGreen
    setMinus colorGreen
    setArrowLeft colorGreen
    setArrowUp colorGreen
    setArrowDown colorGreen
    setArrowRight colorGreen

lightShiftSuper = do
    lightModifiers
    setLeftShift colorWhite
    setLeftSuper colorWhite
    setRightShift colorWhite
    setE colorRed
    setR colorGreen
    setT colorRed
    setT colorGreen
    setP colorGreen
    setEnter colorRed
    setY colorGreen
    setX colorGreen
    setC colorYellow
    setV colorYellow
    setB colorYellow
    setArrowLeft colorGreen
    setArrowUp colorGreen
    setArrowDown colorGreen
    setArrowRight colorGreen

lightAltSuper = do
    lightModifiers
    setLeftAlt colorWhite
    setRightAlt colorWhite
    setLeftSuper colorWhite

lightSuper = do
    lightModifiers
    setLeftSuper colorWhite
    setF8 colorGreen
    setF9 colorGreen
    setQ colorRed
    setW colorRed
    setR colorGreen
    setT colorGreen
    setP colorGreen
    setD colorGreen
    setF colorGreen
    setH colorGreen
    setL colorGreen
    setEnter colorGreen
    setY colorGreen
    setX colorGreen
    setV colorGreen
    setArrowLeft colorGreen
    setArrowUp colorGreen
    setArrowDown colorGreen
    setArrowRight colorGreen

lightAltShift = do
    lightModifiers
    setLeftShift colorWhite
    setLeftAlt colorWhite
    setRightShift colorWhite
    setRightAlt colorWhite
    set1 colorGreen
    set2 colorGreen
    set3 colorGreen
    set4 colorGreen
    set5 colorGreen
    set6 colorGreen
    set7 colorGreen
    set8 colorGreen
    set9 colorGreen
    setArrowLeft colorGreen
    setArrowUp colorGreen
    setArrowDown colorGreen
    setArrowRight colorGreen

lightShift = do
    lightModifiers
    setLeftShift colorWhite
    setRightShift colorWhite
    setF12 colorGreen
    setCircumflex colorGreen
    set1 colorGreen
    set2 colorGreen
    set3 colorGreen
    set4 colorGreen
    set5 colorGreen
    set6 colorGreen
    set7 colorGreen
    set8 colorGreen
    set9 colorYellow
    set0 colorYellow
    setEszett colorGreen
    setBackquote colorGreen
    setTab colorGreen
    setQ colorGreen
    setW colorGreen
    setE colorGreen
    setR colorGreen
    setT colorGreen
    setZ colorGreen
    setU colorGreen
    setI colorGreen
    setO colorGreen
    setP colorGreen
    setUumlaut colorGreen
    setPlus colorGreen
    setA colorGreen
    setS colorGreen
    setD colorGreen
    setF colorGreen
    setG colorGreen
    setH colorGreen
    setJ colorGreen
    setK colorGreen
    setL colorGreen
    setOumlaut colorYellow
    setAumlaut colorGreen
    setHash colorGreen
    setEnter colorGreen
    setLess colorGreen
    setY colorGreen
    setX colorGreen
    setC colorGreen
    setV colorGreen
    setB colorGreen
    setN colorGreen
    setM colorGreen
    setComma colorGreen
    setPeriod colorGreen
    setMinus colorGreen
    setArrowLeft colorGreen
    setArrowUp colorGreen
    setArrowDown colorGreen
    setArrowRight colorGreen

-- Layout for German keyboard. Sorry, you'll have to remap keys for your keyboard.

setEscape = setColor (0,1)
setF1 = setColor (0,2)
setF2 = setColor (0,3)
setF3 = setColor (0,4)
setF4 = setColor (0,5)
setF5 = setColor (0,6)
setF6 = setColor (0,7)
setF7 = setColor (0,8)
setF8 = setColor (0,9)
setF9 = setColor (0,10)
setF10 = setColor (0,11)
setF11 = setColor (0,12)
setF12 = setColor (0,13)
setInsert = setColor (0,14)
setDelete = setColor (0,15)

setCircumflex = setColor (1,1)
set1 = setColor (1,2)
set2 = setColor (1,3)
set3 = setColor (1,4)
set4 = setColor (1,5)
set5 = setColor (1,6)
set6 = setColor (1,7)
set7 = setColor (1,8)
set8 = setColor (1,9)
set9 = setColor (1,10)
set0 = setColor (1,11)
setEszett = setColor (1,12)
setBackquote = setColor (1,13)
setBackspace = setColor (1,15)

setTab = setColor (2,1)
setQ = setColor (2,2)
setW = setColor (2,3)
setE = setColor (2,4)
setR = setColor (2,5)
setT = setColor (2,6)
setZ = setColor (2,7)
setU = setColor (2,8)
setI = setColor (2,9)
setO = setColor (2,10)
setP = setColor (2,11)
setUumlaut = setColor (2,12)
setPlus = setColor (2,13)

setCapsLock = setColor (3,1)
setA = setColor (3,2)
setS = setColor (3,3)
setD = setColor (3,4)
setF = setColor (3,5)
setG = setColor (3,6)
setH = setColor (3,7)
setJ = setColor (3,8)
setK = setColor (3,9)
setL = setColor (3,10)
setOumlaut = setColor (3,11)
setAumlaut = setColor (3,12)
setHash = setColor (3,13)
setEnter = setColor (3,15)

setLeftShift = setColor (4,1)
setLess = setColor (4,2)
setY = setColor (4,3)
setX = setColor (4,4)
setC = setColor (4,5)
setV = setColor (4,6)
setB = setColor (4,7)
setN = setColor (4,8)
setM = setColor (4,9)
setComma = setColor (4,10)
setPeriod = setColor (4,11)
setMinus = setColor (4,12)
setRightShift = setColor (4,15)

setLeftControl = setColor (5,1)
setLeftFn = setColor (5,2)
setLeftSuper = setColor (5,3)
setLeftAlt = setColor (5,4)
setRightAlt = setColor (5,9)
setRightFn = setColor (5,10)
setRightControl = setColor (5,11)
setArrowLeft = setColor (5,12)
setArrowUp = setColor (5,13)
setArrowDown = setColor (5,14)
setArrowRight = setColor (5,15)
