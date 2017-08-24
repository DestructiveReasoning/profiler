import UI.HSCurses.Curses
import System.IO

main :: IO ()
main = do
    initScr
    c <- getch
    let key = decodeKey c
    writeFile "keys" ((show c) ++ " " ++ (show key))
    endWin
