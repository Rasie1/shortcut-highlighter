module Layouts where
import Color

lightModifiers c = do
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
    lightModifiers colorBlue
    setLeftAlt colorWhite
    setRightAlt colorWhite
    setArrowLeft colorGreen
    setArrowRight colorGreen
    setTab colorGreen
    setBackspace colorGreen
    setE colorGreen
    setO colorGreen

lightCtrlShiftSuper = do
    lightModifiers colorBlue
    setLeftControl colorWhite
    setLeftShift colorWhite
    setLeftSuper colorWhite
    setRightControl colorWhite
    setRightShift colorWhite

lightCtrlSuper = do
    lightModifiers colorBlue
    setLeftControl colorWhite
    setLeftSuper colorWhite
    setRightControl colorWhite

lightCtrlAltShift = do
    lightModifiers colorBlue
    setLeftControl colorWhite
    setLeftShift colorWhite
    setLeftAlt colorWhite
    setRightControl colorWhite
    setRightShift colorWhite
    setRightAlt colorWhite

lightCtrlShift = do
    lightModifiers colorBlue
    setLeftControl colorWhite
    setLeftShift colorWhite
    setRightControl colorWhite
    setRightShift colorWhite

lightCtrlAlt = do
    lightModifiers colorBlue
    setLeftControl colorWhite
    setLeftAlt colorWhite
    setRightControl colorWhite
    setRightAlt colorWhite

lightCtrl = do
    lightModifiers colorBlue
    setLeftControl colorWhite
    setRightControl colorWhite

lightShiftSuper = do
    lightModifiers colorBlue
    setLeftShift colorWhite
    setLeftSuper colorWhite
    setRightShift colorWhite

lightAltSuper = do
    lightModifiers colorBlue
    setLeftAlt colorWhite
    setRightAlt colorWhite
    setLeftSuper colorWhite

lightSuper = do
    lightModifiers colorBlue
    setLeftSuper colorWhite

lightAltShift = do
    lightModifiers colorBlue
    setLeftShift colorWhite
    setLeftAlt colorWhite
    setRightShift colorWhite
    setRightAlt colorWhite

lightShift = do
    lightModifiers colorBlue
    setLeftShift colorWhite
    setRightShift colorWhite

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
setDigit0 = setColor (1,2)
setDigit1 = setColor (1,3)
setDigit2 = setColor (1,4)
setDigit3 = setColor (1,5)
setDigit4 = setColor (1,6)
setDigit5 = setColor (1,7)
setDigit6 = setColor (1,8)
setDigit7 = setColor (1,9)
setDigit8 = setColor (1,10)
setDigit9 = setColor (1,11)
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
setEnter = setColor (2,14)

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
