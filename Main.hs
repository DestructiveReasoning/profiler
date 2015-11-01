import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper
import CommanderGeneral
import System.Directory

-- TODO LIST
-- Figure out multiple windows
-- Optimization
-- Scrolling
-- Opening FILES

-- ATTRIBUTES
selected = convertAttributes [Bold]
folder = convertAttributes [Bold]

--COLORS
initColors = do
    initPair (Pair 1) (Color 60) black
    initPair (Pair 2) blue black
    initPair (Pair 3) (Color 235) black
    initPair (Pair 4) (Color 220) black

color60 = Pair 1
colorBlue = Pair 2
color235 = Pair 3
colorYellow = Pair 4

printUnselected :: Window -> [FilePath] -> IO()
printUnselected  _ [] = return()
printUnselected w (p:ps) = do
    (y,x) <- getYX w
    if last p == '/' then wAttrSet w (selected, colorBlue) else wAttrSet w (attr0, (Pair 0))
    mvWAddStr w (y + 1) 0 p
    wAttrSet w (attr0, (Pair 0))
    printUnselected w ps

printSelected :: Window -> FilePath -> IO()
printSelected w fp = do
    (y,_) <- getYX w
    wAttrSet w (selected, colorYellow) 
    mvWAddStr w (y + 1) 0 fp
    wAttrSet w (attr0, (Pair 0))
    return()

printDirectoryList :: Window -> [FilePath] -> Int -> IO()
printDirectoryList _ [] _ = return()
printDirectoryList w pps index = do
    -- Extract selected item from list
    let reg1 = take index pps       --Set of items before the selected item 
        sel  = pps !! index         --Selected item 
        reg2 = drop (index + 1) pps --Set of items after the selected item
    printUnselected w reg1 
    printSelected w sel
    printUnselected w reg2

openFile :: [FilePath] -> Int -> IO Int
openFile [] _ = return 0
openFile list index = do
    if (last (list !! index) == '/') then do
        dir <- getCurrentDirectory
        cd (init (list !! index))
        return 0
    else return index

display :: Window -> Int -> IO()
display w index = do
    (y,_) <- scrSize
    wMove w 0 0 
    dir <- getCurrentDirectory
    wAddStr w dir
    wMove w 1 0
    list <- getDirectoryList dir
    let sortedList = sortDirectoryList list
        viewableList = take (y - 2) sortedList
    printDirectoryList w viewableList index
    update
    refresh
    wclear w
    c <- getCh
    case c of 
        KeyChar 'q' -> return ()
        KeyChar 'h' -> do
            cd ".."
            display w 0
        KeyChar 'j' -> if index < ((length viewableList) - 1) then display w (index + 1) else display w index
        KeyChar 'k' -> if index > 0 then display w (index - 1) else display w index
        KeyChar 'l' -> do 
            index <- openFile viewableList index
            display w index
        KeyChar _   -> display w index

main = do
    initCurses
    initColors
    cursSet CursorInvisible
    echo False
    w <- initScr
    wMove w 20 20 
    display w 0
    endWin
