import UI.HSCurses.Curses
import CommanderGeneral
import System.Directory

-- TODO LIST
-- Selecting
-- Figure out multiple windows
-- Colors

loop :: Window -> IO()
loop w = do
    (y,x) <- getYX w
    wMove w 10 10
    wAddStr w "Hello World"
    wMove w y x
    update
    refresh
    c <- getCh
    (y',x') <- getYX w
    case c of
        KeyChar 'q' -> return ()
        KeyChar 'Q' -> return ()
        KeyChar 'h' -> do
            wMove w y' (x' - 1)
            loop w
        KeyChar 'j' -> do
            wMove w (y' + 1) x'
            loop w 
        KeyChar 'k' -> do
            wMove w (y' - 1) x' 
            loop w
        KeyChar 'l' -> do
            wMove w y' (x' + 1)
            loop w
        KeyChar ch -> do
            wAddStr w (ch:[])
            loop w

printDirectoryList :: Window -> [FilePath] -> IO()
printDirectoryList _ [] = return()
printDirectoryList w (p:ps) = do
    (y,x) <- getYX w
    mvWAddStr w (y + 1) 0 p
    printDirectoryList w ps

display :: Window -> IO()
display w = do
    (y,_) <- scrSize
    wMove w 0 0 
    dir <- getCurrentDirectory
    wAddStr w dir
    wMove w 1 0
    list <- getDirectoryList dir
    let sortedList = sortDirectoryList list
        viewableList = take (y - 2) sortedList
    printDirectoryList w viewableList
    update
    refresh
    wclear w
    c <- getCh
    case c of 
        KeyChar 'q' -> return ()
        KeyChar 'h' -> do
            cd ".."
            display w
        KeyChar _   -> display w

main = do
    initCurses
    echo False
    w <- initScr
    wMove w 20 20 
    display w
    endWin
