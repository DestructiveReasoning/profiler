import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper
import CommanderGeneral
import System.Directory
import System.Process
import System.Exit
import qualified Dispatch as Dispatch

-- TODO LIST
-- Have openFile return (Index, Scroll) for accurate positioning after a file is opened
-- Open with: 
-- Make findPattern return a list of items matching the pattern to implement 'next' function
-- mv, cp, rm
-- Figure out multiple windows
-- Optimization
-- Fix nohup hack
-- Show command "history" after closing, rather than clearing. --> Close like ranger.
-- Give user option to use Dispatch rather than xdg-open
-- Search Todo

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
    (_,x) <- scrSize
    let width = x `div` 3
    let formattedList = map ((\s -> if (length s) < width then s ++ (spaces (width - (length s))) else s) . (truncateFile width)) pps
    -- Extract selected item from list
    let reg1 = take index formattedList       --Set of items before the selected item 
        sel  = formattedList !! index         --Selected item 
        reg2 = drop (index + 1) formattedList --Set of items after the selected item
    -- TODO Add * in front of selected. Call one print function that decides color based on *.
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
--        wclear win
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

calculateIndexScroll :: [FilePath] -> Int -> Int -> (Int,Int)
calculateIndexScroll list height index =
    let scrollThreshold = height `div` 2
        len = length list
        in  if index <= scrollThreshold then (index,0)
            else if index < (len + 3 - height) then (index,index - scrollThreshold)
            else (index,len + 3 - height)

calculateIndexScroll' :: [FilePath] -> Int -> Int -> Int
calculateIndexScroll' list height index =
    let scrollThreshold = height `div` 2
        len = length list
        in  if index <= scrollThreshold then 0
            else if index < (len + 3 - height) then index - scrollThreshold
            else len + 3 - height

search :: Window -> Int -> Int -> Int -> [FilePath] -> String -> IO()
search w x i s dir "" = do
    (y,_) <- scrSize
    wMove w (y - 1) x 
    wAttrSet w (attr0,colorYellow)
    wAddStr w "/"
    wAttrSet w (attr0,(Pair 0))
    display w i s False
    refresh
    c <- getCh
    case c of
        KeyChar '\n' -> display w i s True
        KeyChar q -> if q `elem` fileChar then search w x i s dir ("/" ++ [q]) else search w x i s dir ""
        _ -> search w x i s dir ""
search w x i s dir (p:ps) = do
    (y,_) <- scrSize 
    let index = if findPattern dir ps < (length dir) then findPattern dir ps else (length dir - 1)
        scroll = calculateIndexScroll' dir y index
--    let (index,scroll) = calculateIndexScroll dir y index
    wclear w
    display w (index -scroll) scroll False
    wMove w (y-1) x
    wAttrSet w (attr0,colorYellow)
    wAddStr w (p:ps)
    wAttrSet w (attr0,(Pair 0))
    refresh
    c <- getCh
    case c of
        KeyChar '\b' -> if (length (p:ps)) > 1 then search w x (index - scroll) scroll dir (p:(init ps)) else search w x 0 0 dir ""
        KeyChar '\n' -> display w (index - scroll) scroll True
        KeyChar q -> if q `elem` fileChar then search w x (index - scroll) scroll dir ((p:ps) ++ [q]) else search w x i s dir (p:ps)
        _ -> search w x (index - scroll) scroll dir (p:ps)

display :: Window -> Int -> Int -> Bool -> IO()
display w index scroll prompt= do
    (y,_) <- scrSize
    let scrollThreshold = y `div` 2
    wMove w 0 0 
    dir <- getCurrentDirectory
    wAttrSet w (folder, colorYellow);
    wAddStr w dir
    wAttrSet w (attr0,(Pair 0))
    wMove w 1 0
    list <- getDirectoryList dir
    -- TODO move this code into input parsing to eliminate sorting every frame
    let sortedList = sortDirectoryList list
        len = length sortedList
        viewableList = take (y - 3) $ drop scroll sortedList
    printDirectoryList w viewableList index
    wMove w 0 60
    refresh
    if prompt then do
        wclear w
        c <- getCh
        case c of 
            KeyChar 'q' -> return ()
            KeyChar 'h' -> do
                cd ".."
                display w 0 0 True
            KeyChar 'j' -> do
                if ((index <= scrollThreshold || len <= (y-3)) && index < ((length viewableList) - 1) && scroll == 0) then display w (index + 1) 0 True
                else if ((index <= scrollThreshold || len <= (y-3)) && scroll == 0) then display w (index) 0 True
                else if (scroll < len + 3 - y) then display w index (scroll + 1) True
                else if (index < (length viewableList) - 1) then display w (index + 1) (len + 3 - y) True
                else display w index scroll True
            KeyChar 'k' -> do
                if (len <= (y-3) && index > 0) then display w (index - 1) 0 True
                else if (index > scrollThreshold + 1) then display w (index - 1) (len + 3 - y) True
                else if (scroll > 0) then display w index (scroll - 1) True
                else if index > 0 then display w (index - 1) 0 True
                else display w index scroll True
            KeyChar 'l' -> do --Open file, disowning the process
                index <- openFile w viewableList index True
                display w index 0 True
            KeyChar '/' -> do
                search w 0 index scroll sortedList ""
                
            --Return key
            KeyChar '\n' -> do --Open file, waiting for process to terminate
                index <- openFile w viewableList index False
                display w index 0 True
            _   -> display w index scroll True
    else return()

display' :: Window -> [FilePath] -> Int -> Int -> Bool -> IO()
display' w fpath index scroll prompt= do
    (y,_) <- scrSize
    let scrollThreshold = y `div` 2
    wMove w 0 0 
    dir <- getCurrentDirectory
    wAttrSet w (folder, colorYellow);
    wAddStr w dir
    wAttrSet w (attr0,(Pair 0))
    wMove w 1 0
--    let sortedList = sortDirectoryList list
    let len = length fpath
        viewableList = take (y - 3) $ drop scroll fpath
    printDirectoryList w viewableList index
    wMove w 0 60
    refresh
    if prompt then do
        wclear w
        c <- getCh
        case c of 
            KeyChar 'q' -> return ()
            KeyChar 'h' -> do
                cd ".."
                newdir <- getCurrentDirectory
                newlist <- getDirectoryContents newdir
                let newSortedList = sortDirectoryList newlist
                display' w newSortedList 0 0 True
            KeyChar 'j' -> do
                if ((index <= scrollThreshold || len <= (y-3)) && index < ((length viewableList) - 1) && scroll == 0) then display' w fpath (index + 1) 0 True
                else if ((index <= scrollThreshold || len <= (y-3)) && scroll == 0) then display' w fpath (index) 0 True
                else if (scroll < len + 3 - y) then display' w fpath index (scroll + 1) True
                else if (index < (length viewableList) - 1) then display' w fpath (index + 1) (len + 3 - y) True
                else display' w fpath index scroll True
            KeyChar 'k' -> do
                if (len <= (y-3) && index > 0) then display' w fpath (index - 1) 0 True
                else if (index > scrollThreshold + 1) then display' w fpath (index - 1) (len + 3 - y) True
                else if (scroll > 0) then display' w fpath index (scroll - 1) True
                else if index > 0 then display' w fpath (index - 1) 0 True
                else display' w fpath index scroll True
            KeyChar 'l' -> do --Open file, disowning the process
                index <- openFile w viewableList index True
                newdir <- getCurrentDirectory
                newlist <- getDirectoryContents newdir
                let newSortedList = sortDirectoryList newlist
                display' w newSortedList index scroll True
            KeyChar '/' -> do
                search w 0 index scroll fpath ""
                
            --Return key
            KeyChar '\n' -> do --Open file, waiting for process to terminate
                index <- openFile w viewableList index False
                newdir <- getCurrentDirectory
                newlist <- getDirectoryContents newdir
                let newSortedList = sortDirectoryList newlist
                display' w newSortedList index scroll True

            _   -> display' w fpath index scroll True
    else return()

main = do
    initCurses
    clrs <- colorPairs
    if clrs < 250 then initColors16 else initColors
    cursSet CursorInvisible
    echo False
    w <- initScr
    wMove w 20 20 
    dir <- getCurrentDirectory
    list <- getDirectoryContents dir
    let sortedList = sortDirectoryList list
--    display' w sortedList 0 0 True
    display w 0 0 True
    endWin
    system "clear"
