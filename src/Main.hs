import CommanderGeneral (FileBrowser (..), getDirectoryList)
import System.Directory
import System.Exit
import System.Process
import System.Posix.Process
import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper

data FileMod = MV | CP | MKDIR deriving (Eq, Show)

data Activated = LeftBrowser | RightBrowser

data Profiler = Profiler FileBrowser FileBrowser Activated

modProfiler :: (FileBrowser -> FileBrowser) -> Profiler -> Profiler
modProfiler f (Profiler lbrowser rbrowser LeftBrowser) = Profiler (f lbrowser) rbrowser LeftBrowser
modProfiler f (Profiler lbrowser rbrowser RightBrowser) = Profiler lbrowser (f rbrowser) RightBrowser

-- ATTRIBUTES
selected = convertAttributes [Reverse, Bold]
folder = convertAttributes [Bold]

-- COLORS
initColors = do
    initPair (Pair 1) (Color 60) defaultBackground
    initPair (Pair 2) blue defaultBackground
    initPair (Pair 3) (Color 235) defaultBackground
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

spaces :: Int -> [Char]
spaces x = take x $ repeat ' '

main = do
    initCurses
    colorsSupported <- colorPairs
    if colorsSupported < 250 then initColors16 else initColors
    cursSet CursorInvisible
    echo False
    w <- initScr
    (y,x) <- scrSize
    wleft <- newWin (y - marginSize) ((x `div` 2) - marginSize) 0 marginSize
    wRefresh wleft
    wright <- newWin (y - marginSize) ((x `div` 2) - marginSize) 0 (x `div` 2 + marginSize)
    wRefresh wright
    update
    dir <- getCurrentDirectory
    list <- getDirectoryList dir
    let leftPane    = FileBrowser {window=wleft, directory=dir, files=list, indexStack=[0]}
        rightPane   = FileBrowser {window=wright, directory=dir, files=list, indexStack=[0]}
        profiler    = Profiler leftPane rightPane LeftBrowser
    putStrLn "Hello, Newman"
    endWin
