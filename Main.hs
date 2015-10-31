import UI.HSCurses.Curses
import CommanderGeneral

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

main = do
    initCurses
    echo False
    w <- initScr
    wMove w 20 20 
    loop w
    endWin
