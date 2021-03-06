import CommanderGeneral
import System.Directory
import System.Exit
import System.Process
import System.Posix.Process
import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper

-- TODO LIST                                                STATUS
-- User-defined color schemes                               Not started
-- Visual Mode                                              Not started
-- Copy/Paste                                               Not started
-- Try scrolling without bounds using diplay'               ?
-- Fix nohup hack                                           Try using makeProperDirectory
-- Give openWith capability to spawn terminal applications  Not started
-- Search for patterns in middle of file name?              Not started
-- Give user option to use Dispatch rather than xdg-open    Already done, needs to be implemented
-- Optimization                                             Try reducing calls of wclear in display and display'
-- Different colors for inactive panel                      Not started
-- Search Todo

-- ERRORS TO FIX
-- Backspace not working                                    No progress
-- Resizing terminal                                        Not started
-- Test resizing with a floating WM                         Not started
-- Index shift after searching                              No progress
-- Fix performance in color-unfriendly terminals            Not started
-- Test out next in search function                         So far so good
-- Fix command history upon close.                          Mostly working, sometimes buggy.
-- Fix reapparition of cursor after opening CLI program     Tried invoking cursSet in display functions.

-- DATA TYPES
data FileManip = MV|CP|MKDIR deriving (Eq)

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

marginSize :: Int
marginSize = 2

run :: Window -> Window -> Int -> Int -> Int -> Int -> IO()
run w1 w2 i1 s1 i2 s2 = do
--    dir2 <- display w2 i2 s2 False
    dir <- getCurrentDirectory
    list <- getDirectoryList dir
    let sortedList = sortDirectoryList list
--    wRefresh w2
--    (index1, scroll1, dir1, quit) <- display' w1 sortedList i1 s1 "" True
--    mvWAddStr w1 0 0 "CYCLING"
--    cycle w2 w1 i2 s2 index1 scroll1 dir2 dir1 quit
    cycle w1 w2 i1 s1 i2 s2 dir dir False
    where   cycle win1 win2 index1 scroll1 index2 scroll2 d1 d2 q = do
                if q then return()
                else do
                    cd d2
                    dir2 <- display win2 index2 scroll2 False
                    cd d1
                    dir' <- getCurrentDirectory
                    list' <- getDirectoryList dir'
                    let sortedList' = sortDirectoryList list'
                    (i1,s1,dir1,quit) <- display' win1 sortedList' index1 scroll1  "" True
--                    wRefresh win1
--                    wRefresh win2
--                    refresh
--                    update 
                    cycle win2 win1 index2 scroll2 i1 s1 dir2 dir1 quit

spaces :: Int -> [Char]
spaces 0 = []
spaces x = ' ':(spaces (x - 1))

printUnselected :: Window -> [FilePath] -> IO()
printUnselected  _ [] = return()
printUnselected w (p:ps) = do
    (y,x) <- getYX w
    if lastChar p == '/' then wAttrSet w (folder, colorBlue) else wAttrSet w (attr0, (Pair 0))
    mvWAddStr w (y + 1) 1 p
    wAttrSet w (attr0, (Pair 0))
    printUnselected w ps

printSelected :: Window -> FilePath -> IO()
printSelected w fp = do
    (y,_) <- getYX w
    wAttrSet w (selected, colorBlue) 
    mvWAddStr w (y + 1) 1 fp
    wAttrSet w (attr0, (Pair 0))
    return()

printDirectoryList :: Window -> [FilePath] -> Int -> IO()
printDirectoryList _ [] _ = return()
printDirectoryList w pps index = do
    (_,x) <- scrSize
    let width = x `div` 2 - 2 - marginSize
    let formattedList = map ((\s -> if (length s) < width then s ++ (spaces (width - (length s))) else s) . (truncateFile width)) pps
    -- Extract selected item from list
    let reg1 = take index formattedList       --Set of items before the selected item 
        sel  = formattedList !! index         --Selected item 
        reg2 = drop (index + 1) formattedList --Set of items after the selected item
    -- TODO Add * in front of selected. Call one print function that decides color based on *.
    printUnselected w reg1 
    printSelected w sel
    printUnselected w reg2

-- For testing the efficiency of printDirectoryList, this is not a substitute
printDirectoryList':: Window -> [FilePath] -> Int -> IO()
printDirectoryList' _ [] _ = return ()
printDirectoryList' w (p:ps) index = do
    (y,_) <- getYX w
    mvWAddStr w (y + 1) 0 p 
    return()

mv :: Window -> Int -> Int -> FilePath -> FilePath -> FileManip -> IO()
mv w index scroll file newPath copy = do
    (totaly,_) <- scrSize
    let y = totaly - marginSize
    dir <- getCurrentDirectory
    home <- getHomeDirectory
    werase w 
    display w index scroll False
    wMove w (y - 2) (1) 
    if copy == CP then wAddStr w $ "Copy to: " ++ newPath
    else if copy == MV then wAddStr w $ "Move to: " ++ newPath
    else wAddStr w $ "Create new directory: " ++ newPath
    display w index scroll False
    wRefresh w
    c <- getCh
    case c of
        KeyChar '\b' ->
            if (length newPath < 1) then mv w index scroll file "" copy
            else mv w index scroll file (init newPath) copy
        KeyChar '\n' -> do
            let newPath' =  if (head newPath == '~') then (home ++ "/" ++ (tail newPath))
                            else if (length newPath) < 2 then file
                            else if (head newPath == '/') then newPath
                            else dir ++ "/" ++ newPath
                finalPath = makeProperDirectory newPath'
            if copy == CP then system $ "cp " ++ (makeProperDirectory dir) ++ "/" ++ file ++ " " ++ finalPath
            else if copy == MV then system $ "mv " ++ (makeProperDirectory dir) ++ "/" ++ file ++ " " ++ finalPath
            else system $ "mkdir " ++ finalPath ++ "/"
            werase w
            wnoutRefresh w
            update
            return()
        KeyChar q ->
            if q `elem` fileChar then mv w index scroll file (newPath ++ [q]) copy
            else mv w index scroll file newPath copy
        _ -> mv w index scroll file newPath copy

rm :: Window -> Int -> Int -> FilePath -> IO ()
rm w index scroll file = do
    (totaly,_) <- scrSize
    let y = totaly - marginSize
    dir <- getCurrentDirectory
    werase w 
    display w index scroll False
    wMove w (y - 2) (1)
    wAddStr w $ "Remove " ++ file ++ "? [y/N]"
    wRefresh w
    c <- getCh
    case c of 
        KeyChar 'y' -> do
            system $ "rm " ++ (makeProperDirectory dir) ++ "/" ++ file
            werase w
            return()
        _ -> do werase w; return()

openWith :: Window -> [FilePath] -> Int -> Int -> String -> IO ()
openWith w filepath index scroll prog = do
    (totaly,_) <- scrSize
    let y = totaly - marginSize
    dir' <- getCurrentDirectory
    werase w
    display w index scroll False
    wMove w (y - 2) (1)
    wAttrSet w (attr0,colorYellow)
    if(findPattern [prog] "not found" == 0) then do 
        let prog' = "" 
        wAddStr w (prog)
        return()
    else do 
        let prog' = prog 
        wAddStr w ("Open with: " ++ prog')
        wAttrSet w (attr0,(Pair 0))
        display w index scroll False
        res <- findExecutable prog
        wRefresh w
        c <- getCh
        case c of 
            KeyChar '\b'    ->
                if (length prog < 1) then return()
                else do openWith w filepath index scroll (init prog) 
            KeyChar '\n'    ->
                -- Open file with desired program
                if (length prog < 1) then do werase w; return()
                else if res == Nothing then openWith w filepath index scroll ("not found: " ++ prog)
                else do
                    handle <- spawnProcess "nohup" [prog,(filepath !! index)]
                    system $ "rm " ++ (makeProperDirectory dir') ++ "/nohup.out"
                    werase w
                    wRefresh w
                    return()
            KeyChar q       ->
                if q `elem` fileChar then openWith w filepath index scroll (prog ++ [q])
                else openWith w filepath index scroll prog
            _               -> openWith w filepath index scroll prog

openFile' :: Window -> [FilePath] -> Int -> Int -> Bool -> IO (Int,Int)
openFile' _ [] index scroll _ = return (index,scroll)
openFile' win list index scroll decouple = do
    dir <- getCurrentDirectory
    -- Formatting filepath to be parsed by the system command on line 109.
    let dir' = makeProperDirectory dir
    if (last (list !! index) == '/') then do
        cd (init (list !! index))
        return (0,0)
    else do
        let file = list !! index
        if decouple then do 
--            handle <- spawnProcess "nohup" ["xdg-open",(makeProperDirectory file)]
            handle <- spawnProcess "nohup" ["xdg-open",(file)]
            system $ "rm " ++ dir' ++ "/nohup.out"
            werase win
            cursSet CursorInvisible
            wRefresh win
            return (index,scroll)
        else do 
            callProcess "xdg-open" [file]
            werase win
            wRefresh win
            cursSet CursorInvisible
            return (index,scroll)
--        werase win
        wRefresh win
        cursSet CursorInvisible
        return (index,scroll)

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
        in  if index <= scrollThreshold || len < height then 0
            else if index < (len + 3 - height) then index - scrollThreshold
            else len + 3 - height

search :: Window -> Int -> Int -> Int -> [FilePath] -> String -> Bool -> Bool -> IO()
search w x i s dir "" forward prompt = do
    (totaly,_) <- scrSize
    let y = totaly - marginSize - 1
    wMove w (y - 1) (x + 1)
    wAttrSet w (attr0,colorYellow)
    wAddStr w "/"
    wAttrSet w (attr0,(Pair 0))
    display' w dir i s "" False
    wnoutRefresh w
    update
    if prompt then do
        c <- getCh
        case c of
            KeyChar '\n' -> do werase w; display' w dir i s "" True; return()
            KeyChar q -> if q `elem` fileChar then search w x i s dir ("/" ++ [q]) forward True else search w x i s dir "" forward True
            _ -> search w x i s dir "" forward True
    else do display' w dir i s "" True; return()
search w x i s dir (p:ps) forward prompt = do
    (totaly,_) <- scrSize 
    let y = totaly - marginSize - 1
    let index = if findPattern dir ps < (length dir) then findPattern dir ps else (length dir - 1)
        indices = if forward then getListFromPattern dir ps else reverse $ getListFromPattern dir ps
        nextIndices =   if not prompt then 
                            if forward then if (length indices == 0) then [i] else if (last indices) <= i + s then indices else dropWhile (<= i + s) indices
                            else if (length indices == 0) then [i] else if (last indices) >= i + s then indices else dropWhile (>= i + s) indices
                        else  
                            if forward then if (length indices == 0) then [i] else if (last indices) < i + s then indices else dropWhile (< i + s) indices
                            else if (length indices == 0) then [i] else if (last indices) > i + s then indices else dropWhile (> i + s) indices
        index' = head nextIndices
        scroll = calculateIndexScroll' dir (y-1) index'
--    let (index,scroll) = calculateIndexScroll dir y index
    werase w
    display' w dir (index' -scroll) scroll "" False
    wMove w (y-1) (x + 1)
    wAttrSet w (attr0,colorYellow)
    wAddStr w (p:ps)
    wAttrSet w (attr0,(Pair 0))
    wnoutRefresh w
    update
    if prompt then do
        c <- getCh
        case c of
--            KeyChar '\b' -> if (length (p:ps)) > 1 then search w x (index' - scroll) scroll dir (p:(init ps)) forward True else search w x 0 0 dir "" forward True
            KeyChar '\b' -> if (length (p:ps)) > 1 then search w x (index' - scroll) scroll dir (p:(init ps)) forward True else search w x 0 0 dir "" forward True
            KeyF 127-> if (length (p:ps)) > 1 then search w x (index' - scroll) scroll dir (p:(init ps)) forward True else search w x 0 0 dir "" forward True
            KeyChar '\n' -> do werase w; display' w dir (index' - scroll) scroll (p:ps) True; return()
            KeyChar q -> if q `elem` fileChar then search w x (index' - scroll) scroll dir ((p:ps) ++ [q]) forward True else search w x i s dir (p:ps) forward True
            _ -> search w x (index' - scroll) scroll dir (p:ps) forward True
    else do display' w dir (index' - scroll) scroll (p:ps) True; return()

display :: Window -> Int -> Int -> Bool -> IO FilePath
display w index scroll prompt= do
    cursSet CursorInvisible
    refresh
    (totaly,_) <- scrSize
    let y = totaly - marginSize - 1
    let scrollThreshold = y `div` 2
    wMove w (marginSize - 1) 1 
    dir <- getCurrentDirectory
    wAttrSet w (folder, colorYellow);
    wAddStr w dir
    wAttrSet w (attr0,(Pair 0))
    wMove w (marginSize) 0
    list <- getDirectoryList dir
    -- TODO move this code into input parsing to eliminate sorting every frame
    let sortedList = sortDirectoryList list
        len = length sortedList
        viewableList = take (y - 4) $ drop scroll sortedList
    printDirectoryList w viewableList index
--    wAttrSet w (folder, colorYellow);
--    wBorder w defaultBorder
--    wAttrSet w (attr0, (Pair 0));
    wRefresh w
    if prompt then do
        wclear w
        c <- getCh
        case c of 
            KeyChar 'q' -> return dir
            KeyChar 'h' -> do
                cd ".."
                display w 0 0 True
            KeyChar 'j' -> do
                if ((index <= scrollThreshold || len <= (y-4)) && index < ((length viewableList) - 1) && scroll == 0) then display w (index + 1) 0 True
                else if ((index <= scrollThreshold || len <= (y-4)) && scroll == 0) then display w (index) 0 True
                else if (scroll < len + 4 - y) then display w index (scroll + 1) True
                else if (index < (length viewableList) - 1) then display w (index + 1) (len + 4 - y) True
                else display w index scroll True
            KeyChar 'k' -> do
                if (len <= (y-4) && index > 0) then display w (index - 1) 0 True
                else if (index > scrollThreshold + 1) then display w (index - 1) (len + 4 - y) True
                else if (scroll > 0) then display w index (scroll - 1) True
                else if index > 0 then display w (index - 1) 0 True
                else display w index scroll True
            KeyChar 'l' -> do --Open file, disowning the process
                (index',scroll') <- openFile' w viewableList index scroll True
                display w index 0 True
            KeyChar '/' -> do
                search w 0 index scroll sortedList "" True True
                newdir <- getCurrentDirectory
                return dir
                
            --Return key
            KeyChar '\n' -> do --Open file, waiting for process to terminate
                (index',scroll') <- openFile' w viewableList index scroll False
                display w index 0 True
            _   -> display w index scroll True
    else do wRefresh w; return dir

display' :: Window -> [FilePath] -> Int -> Int -> String -> Bool -> IO(Int,Int,FilePath,Bool)
display' w fpath index scroll lastSearch prompt= do
    cursSet CursorInvisible
--    refresh
--    wnoutRefresh w
    (totaly,_) <- scrSize
    let y = totaly - marginSize - 1
    let scrollThreshold = y `div` 2
    wMove w (marginSize-1) 1 
    dir <- getCurrentDirectory
    wAttrSet w (folder, colorYellow);
    wAddStr w dir
    wAttrSet w (attr0,(Pair 0))
    wMove w (marginSize) 0
--    let sortedList = sortDirectoryList list
    let len = length fpath
        viewableList = take (y - 4) $ drop scroll fpath
    printDirectoryList w viewableList index
--    wMove w 0 60
    wAttrSet w (folder, colorYellow);
    wBorder w defaultBorder
    wAttrSet w (attr0,(Pair 0))
    if prompt then do
        wnoutRefresh w
        werase w
        update
--        wclear w
        c <- getCh
        case c of 
            KeyChar 'q' -> return (0,0,"",True)
            KeyChar 'h' -> do
                cd ".."
                newdir <- getCurrentDirectory
                newlist <- getDirectoryList newdir
                let newSortedList = sortDirectoryList newlist
                display' w newSortedList 0 0 lastSearch True
                return (0,0,newdir,False)
            KeyChar 'j' -> do
                if ((index <= scrollThreshold || len <= (y-4)) && index < ((length viewableList) - 1) && scroll == 0) then display' w fpath (index + 1) 0 lastSearch True
                else if ((index <= scrollThreshold || len <= (y-4)) && scroll == 0) then display' w fpath (index) 0 lastSearch True
                else if (scroll < len + 4 - y) then display' w fpath index (scroll + 1) lastSearch True
                else if (index < (length viewableList) - 1) then display' w fpath (index + 1) (len + 4 - y) lastSearch True
                else display' w fpath index scroll lastSearch True
            KeyChar 'k' -> do
                if (len <= (y-4) && index > 0) then display' w fpath (index - 1) 0 lastSearch True
                else if (index > scrollThreshold + 1) then display' w fpath (index - 1) (len + 4 - y) lastSearch True
                else if (scroll > 0) then display' w fpath index (scroll - 1) lastSearch True
                else if index > 0 then display' w fpath (index - 1) 0 lastSearch True
                else display' w fpath index scroll lastSearch True
            KeyChar 'l' -> do --Open file, disowning the process
                (index',scroll') <- openFile' w viewableList index scroll True
                newdir <- getCurrentDirectory
                newlist <- getDirectoryList newdir
                let newSortedList = sortDirectoryList newlist
                display' w newSortedList index' scroll' lastSearch True
            KeyChar '0' -> display' w fpath 0 0 lastSearch True
            KeyChar '/' -> do
                search w 0 index scroll fpath "" True True
                newdir <- getCurrentDirectory
                return (index,scroll,newdir,False)
            KeyChar 'n' -> do
                search w 0 (index) scroll fpath lastSearch True False
                newdir <- getCurrentDirectory
                return (index,scroll,newdir,False)
            KeyChar 'N' -> do
                search w 0 (index) scroll fpath lastSearch False False
                newdir <- getCurrentDirectory
                return (index,scroll,newdir,False)
                
            --Return key
            KeyChar '\n' -> do --Open file, waiting for process to terminate
                (index',scroll') <- openFile' w viewableList index scroll False
                newdir <- getCurrentDirectory
                newlist <- getDirectoryList newdir
                let newSortedList = sortDirectoryList newlist
                display' w newSortedList index' scroll' lastSearch True

            KeyChar 'o' -> do
                openWith w (viewableList) index scroll ""
                let newSortedList = sortDirectoryList fpath
                display' w newSortedList index scroll lastSearch True

            KeyChar 'S' -> do
                mv w index scroll (viewableList !! index) "" MV
                newdir <- getCurrentDirectory
                newlist <- getDirectoryList newdir
                let newSortedList = sortDirectoryList newlist
                display' w newSortedList index scroll lastSearch True
            KeyChar 'y' -> do
                mv w index scroll (viewableList !! index) "" CP
                newdir <- getCurrentDirectory
                newlist <- getDirectoryList newdir
                let newSortedList = sortDirectoryList newlist
                display' w newSortedList index scroll lastSearch True
            KeyChar 'O' -> do
                mv w index scroll (viewableList !! index) "" MKDIR
                newdir <- getCurrentDirectory
                newlist <- getDirectoryList newdir
                let newSortedList = sortDirectoryList newlist
                display' w newSortedList index scroll lastSearch True
            KeyChar 'd' -> do
                rm w index scroll (viewableList !! index) 
                newdir <- getCurrentDirectory
                newlist <- getDirectoryList newdir
                let newSortedList = sortDirectoryList newlist
                    scroll' = calculateIndexScroll' newSortedList y (index + scroll)
                    index' = 
                        if (index + scroll') >= (length newSortedList) - 1 then (length newSortedList) - 1
                        else index
                display' w newSortedList index' scroll' lastSearch True
            KeyChar '^' -> do
                home <- getHomeDirectory
                cd home
                newdir <- getCurrentDirectory
                newlist <- getDirectoryList newdir
                let newSortedList = sortDirectoryList newlist
                display' w newSortedList 0 0 lastSearch True
            KeyChar 'G' -> do
                let index' = (length fpath - 1)
                    scroll' = calculateIndexScroll' fpath y index'
                display' w fpath ((length viewableList) - 1) (len + 3 - y) lastSearch True
            KeyChar '\t' -> do
                newdir <- getCurrentDirectory
                let scroll' = calculateIndexScroll' fpath y index
                return (index,scroll,newdir,False)
            _   -> display' w fpath index scroll lastSearch True
    else do
        return(0,0,"",False)

main = do
    initCurses
    clrs <- colorPairs
    if clrs < 250 then initColors16 else initColors
    cursSet CursorInvisible
    echo False
    w <- initScr
    (y,x) <- scrSize
--    w1 <- newwin y (x `div` 2) 0 0
    w1 <- newWin (y - marginSize) ((x `div` 2) - marginSize) 0 marginSize
    wRefresh w1
    w2 <- newWin (y - marginSize) ((x `div` 2) - marginSize) 0 (x `div` 2 + marginSize)
    wRefresh w2
    update 
    wMove w1 20 20 
    dir <- getCurrentDirectory
    list <- getDirectoryList dir
    let sortedList = sortDirectoryList list
--    display' w1 sortedList 0 0 "" True
    run w1 w2 0 0 0 0 
    endWin
