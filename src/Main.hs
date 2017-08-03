import CommanderGeneral
import System.Directory (getCurrentDirectory)
import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper

data FileMod = MV | CP | MKDIR deriving (Eq, Show)

data Activated = LeftBrowser | RightBrowser

data Profiler = Profiler FileBrowser FileBrowser Activated

-- Stores list scroll data.
-- First parameter: index to start displaying list
-- Second parameter: index to stop displaying list (exclusive)
-- Third parameter: index within modified list that is selected
data ScrollIndex a = ScrollIndex a a a

modProfiler :: (FileBrowser -> FileBrowser) -> Profiler -> Profiler
modProfiler f (Profiler lbrowser rbrowser LeftBrowser) = Profiler (f lbrowser) rbrowser LeftBrowser
modProfiler f (Profiler lbrowser rbrowser RightBrowser) = Profiler lbrowser (f rbrowser) RightBrowser

-- ATTRIBUTES
selected = convertAttributes [Reverse, Bold]
folder = convertAttributes [Bold]

-- Specifies margin on windows
marginSize :: Int
marginSize = 2

-- Returns the total amount of files that can be listed in a window
fileCapacity :: IO Int
fileCapacity = fst <$> scrSize >>= (\y -> pure(y - marginSize))

-- Returns total amount of characters that can be displayed in a window line
fileDisplayLength :: IO Int
fileDisplayLength = snd <$> scrSize >>= (\x -> pure(x `div` 2 - marginSize))

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

-- Clears formatting and color attributes of window
wClearAttribs :: Window -> IO ()
wClearAttribs window = wAttrSet window (attr0, Pair 0)

-- Calculates indices to display from list
getDisplayListIndices :: FileBrowser -> IO (ScrollIndex Int)
getDisplayListIndices browser = do
    cap <- fileCapacity
    let len         = length $ files browser
        i           = head $ indexStack browser
        threshold   = cap `div` 2
    if (len <= cap) then return $ ScrollIndex 0 len i
    else if (i < threshold) then return $ ScrollIndex 0 cap i
    else if (i < len - threshold) then return $ ScrollIndex (i - threshold) (i + threshold) threshold
    else return $ ScrollIndex (len - cap) len (i + cap - len)

-- Displays FileBrowser contents
displayBrowser :: FileBrowser -> IO ()
displayBrowser browser = do
    cursSet CursorInvisible
    let w           = window browser
        wholeDir    = files browser 
    wMove w (marginSize - 1) 1
    wAttrSet w (folder, colorYellow)
    wAddStr w (directory browser)
    wClearAttribs w
    wMove w marginSize 0
    showFileList browser
    wRefresh w

showFileList :: FileBrowser -> IO ()
showFileList browser = 
    getDisplayListIndices browser >>= (\(ScrollIndex s e i) -> showList i 0 (sliceList s e (files browser)))
    where   showList _ _ [] = return ()
            showList sel cur (x:xs) = 
               showFile (sel == cur) x >> showList sel (cur + 1) xs
            showFile isSelected file = 
                let w = window browser
                    attrib = 
                        if (isSelected) then (selected, colorBlue)
                        else if ((last file) == '/') then (folder, colorBlue)
                        else (attr0, (Pair 0))
                    in do
                        (y,_) <- getYX w
                        limit <- fileDisplayLength
                        let truncated   = truncateFileName limit file
                            file'       = truncated ++ (spaces (limit - (length truncated)))
                        wAttrSet w attrib >> mvWAddStr w (y+1) 1 file' >> wClearAttribs w

-- Generates string of spaces
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
    list <- sortDirectoryList <$> getDirectoryList dir
    let leftPane    = FileBrowser {window=wleft, directory=dir, files=list, indexStack=[0]}
        rightPane   = FileBrowser {window=wright, directory=dir, files=list, indexStack=[0]}
        profiler    = Profiler leftPane rightPane LeftBrowser
    displayBrowser leftPane
    c <- getChar
    endWin
