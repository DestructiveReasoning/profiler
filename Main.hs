import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper
import CommanderGeneral
import System.Directory
import System.Process
import qualified Dispatch as Dispatch

-- TODO LIST
-- Truncation of filepaths
-- Open with: 
-- Searching
-- mv, cp, rm
-- Figure out multiple windows
-- Optimization
-- Fix nohup hack
-- Show command "history" after closing, rather than clearing. --> Close like ranger.
-- Give user option to use Dispatch rather than xdg-open

-- ERRORS TO FIX
-- Fix index shift after opening file
-- Fix performance in color-unfriendly terminals

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

openFile :: Window -> [FilePath] -> Int -> Bool -> IO Int
openFile _ [] _ _ = return 0
openFile win list index decouple = do
    dir <- getCurrentDirectory
    -- Formatting filepath to be parsed by the system command on line 109.
    let dir' = ((formatDirectory ' ' "\\ ") . (formatDirectory '\'' "\\'")) dir
    if (last (list !! index) == '/') then do
        cd (init (list !! index))
        return 0
    else do
        let file = list !! index
        if decouple then do 
            handle <- spawnProcess "nohup" ["xdg-open",file]
            system $ "rm " ++ dir' ++ "/nohup.out"
            wclear win
            refresh
            return index
        else do 
            callProcess "xdg-open" [file]
            wclear win
            cursSet CursorInvisible
            refresh
            return index
        wclear win
        cursSet CursorInvisible
        refresh
        return index
--    else do
--        let extension = dropWhile (/= '.') (list !! index)
--            program = if (not (Dispatch.isCodeFile extension)) then (head $ Dispatch.procedures extension) else (head $ Dispatch.procedures "code")
--            file = list !! index
--        handle <- spawnProcess "nohup" [program, file]
--        system $ program ++ " " ++ file
--        wclear win
--        refresh
--        wclear win
--        refresh
--        return index
--    else return index

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
        KeyChar 'l' -> do --Open file, disowning the process
            index <- openFile w viewableList index True
            display w index 0
        --Return key
        KeyChar '\n' -> do --Open file, waiting for process to terminate
            index <- openFile w viewableList index False
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
    system "clear"
