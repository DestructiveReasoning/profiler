import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper
import CommanderGeneral
import System.Directory

-- TODO LIST
-- Scrolling
-- Opening FILES
-- Searching
-- mv, cp, rm
-- Figure out multiple windows
-- Test symlinks
-- Fix performance in color-unfriendly terminals
-- Optimization

-- ATTRIBUTES
selected = convertAttributes [Reverse,Bold]
folder = convertAttributes [Bold]

--COLORS
initColors = do
    initPair (Pair 1) (Color 60) defaultBackground
    initPair (Pair 2) blue defaultBackground
    initPair (Pair 3) (Color 235) defaultBackground
    initPair (Pair 4) (Color 220) defaultBackground
    initPair (Pair 5) (Color 179) defaultBackground

initColors16 = do
    initPair (Pair 1) blue defaultBackground
    initPair (Pair 2) blue defaultBackground
    initPair (Pair 3) magenta defaultBackground
    initPair (Pair 4) yellow defaultBackground
    initPair (Pair 5) green defaultBackground

color60 = Pair 1
colorBlue = Pair 2
color235 = Pair 3
colorYellow = Pair 4
color179 = Pair 5
colorGreen = Pair 5
colorMagenta = Pair 3

spaces :: Int -> [Char]
spaces 0 = []
spaces x = ' ':(spaces (x - 1))

lastChar :: [Char] -> Char
lastChar [] = ' '
lastChar xs = head $ (dropWhile (== ' ') . reverse) xs

printUnselected :: Window -> [FilePath] -> IO()
printUnselected  _ [] = return()
printUnselected w (p:ps) = do
    (y,x) <- getYX w
    if lastChar p == '/' then wAttrSet w (folder, colorBlue) else wAttrSet w (attr0, (Pair 0))
    mvWAddStr w (y + 1) 0 p
    wAttrSet w (attr0, (Pair 0))
    printUnselected w ps

printSelected :: Window -> FilePath -> IO()
printSelected w fp = do
    (y,_) <- getYX w
    wAttrSet w (selected, colorBlue) 
    mvWAddStr w (y + 1) 0 fp
    wAttrSet w (attr0, (Pair 0))
    return()

printDirectoryList :: Window -> [FilePath] -> Int -> IO()
printDirectoryList _ [] _ = return()
printDirectoryList w pps index = do
    -- Extract selected item from list
    (_,x) <- scrSize
    let formattedList = map (\s -> if (length s) < (x `div` 3) then s ++ (spaces ((x `div` 3) - (length s))) else s) pps
    let reg1 = take index formattedList       --Set of items before the selected item 
        sel  = formattedList !! index         --Selected item 
        reg2 = drop (index + 1) formattedList --Set of items after the selected item
    printUnselected w reg1 
    printSelected w sel
    printUnselected w reg2

printDirectoryList':: Window -> [FilePath] -> Int -> IO()
printDirectoryList' _ [] _ = return ()
printDirectoryList' w (p:ps) index = do
    (y,_) <- getYX w
    mvWAddStr w (y + 1) 0 p 
    return()

openFile :: [FilePath] -> Int -> IO Int
openFile [] _ = return 0
openFile list index = do
    if (last (list !! index) == '/') then do
        dir <- getCurrentDirectory
        cd (init (list !! index))
        return 0
    else return index

display :: Window -> Int -> Int -> IO()
display w index scroll = do
    (y,_) <- scrSize
    let scrollThreshold = y `div` 2
    wMove w 0 0 
    dir <- getCurrentDirectory
    wAttrSet w (folder, colorYellow);
    wAddStr w dir
    wAttrSet w (attr0,(Pair 0))
    wMove w 1 0
    list <- getDirectoryList dir
    let sortedList = sortDirectoryList list
        len = length sortedList
        viewableList = take (y - 2) $ drop scroll sortedList
    printDirectoryList w viewableList index
    wMove w 0 60
    wAddStr w (show (len))
--    update
    refresh
    wclear w
    c <- getCh
    case c of 
        KeyChar 'q' -> return ()
        KeyChar 'h' -> do
            cd ".."
            display w 0 0
        KeyChar 'j' -> do
            if ((index <= scrollThreshold || len <= (y-2)) && index < ((length viewableList) - 1)) then display w (index + 1) 0
            else if ((index <= scrollThreshold || len <= (y-2))) then display w (index) 0
            else if (scroll < len + 2 - y) then display w index (scroll + 1)
            else if (index < (length viewableList) - 1) then display w (index + 1) (len + 2 - y)
            else display w index scroll
        KeyChar 'k' -> do
            if (len <= (y-2) && index > 0) then display w (index - 1) 0
            else if (index > scrollThreshold + 1) then display w (index - 1) (len + 2 - y)
            else if (scroll > 0) then display w index (scroll - 1)
            else if index > 0 then display w (index - 1) 0
            else display w index scroll
        KeyChar 'l' -> do 
            index <- openFile viewableList index
            display w index 0
        _   -> display w index 0

main = do
    initCurses
    clrs <- colorPairs
    if clrs < 250 then initColors16 else initColors
    cursSet CursorInvisible
    echo False
    w <- initScr
    wMove w 20 20 
    display w 0 0
    endWin
