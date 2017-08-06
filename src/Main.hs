import CommanderGeneral
import Data.Maybe
import Dispatch
import System.Directory (doesFileExist, getCurrentDirectory, getHomeDirectory)
import System.IO
import System.Posix.Signals
import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper

data ProfilerMode = Normal | Search | Visual | Insert deriving (Show, Eq)

data FileMod = MV | CP | MKDIR deriving (Eq, Show)

data Orientation = NormalOri | FlippedOri deriving (Eq)

data WindowSet = WindowSet {
    active      :: FileBrowser,
    passive     :: FileBrowser,
    orientation :: Orientation
}

data Profiler = Profiler WindowSet ProfilerMode Dispatch

-- Stores list scroll data.
-- First parameter: index to start displaying list
-- Second parameter: index to stop displaying list (exclusive)
-- Third parameter: index within modified list that is selected
data ScrollIndex a = ScrollIndex a a a

switch :: Orientation -> Orientation
switch NormalOri = FlippedOri
switch FlippedOri = NormalOri

flipWindowSet :: WindowSet -> WindowSet
flipWindowSet set = set{active=(passive set), passive=(active set), orientation=(switch (orientation set))}

-- ATTRIBUTES
selected = convertAttributes [Reverse, Bold]
folder = convertAttributes [Bold]

-- Specifies margin on windows
marginSize :: Int
marginSize = 2

-- Returns the total amount of files that can be listed in a window
fileCapacity :: IO Int
fileCapacity = fst <$> scrSize >>= (\y -> pure(y - 3*marginSize))

-- Returns total amount of characters that can be displayed in a window line
fileDisplayLength :: IO Int
fileDisplayLength = snd <$> scrSize >>= (\x -> pure(x `div` 2 - 2*marginSize))

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

run :: Profiler -> IO ()
run profiler = 
    refreshProfiler profiler >>= (\p -> clear p >> render p >> getCh >>= handleInput p)

refreshProfiler :: Profiler -> IO Profiler
refreshProfiler (Profiler set mode disp) = do
    wl <- createLeftWindow
    wr <- createRightWindow
    let act = active set
        pas = passive set
        ori = orientation set
        set' =  if ori == NormalOri then set{active=act{window=wl}, passive=pas{window=wr}}
                else set{active=act{window=wr}, passive=pas{window=wl}}
    pure $ Profiler set' mode disp

handleInput :: Profiler -> Key -> IO ()
handleInput (Profiler set Normal dispatch) input = 
    case input of
        KeyChar '\t'    -> run $ Profiler (flipWindowSet set) Normal dispatch
        KeyChar 'j'     -> 
            let (loc:rest)  = indexStack . active $ set
                fileList    = files . active $ set
                index'      = if (loc + 1 >= length fileList) then loc else loc + 1
                browser     = (active set){indexStack=(index':rest)}
            in run $ Profiler set{active=browser} Normal dispatch
        KeyChar 'k'     -> 
            let (loc:rest)  = indexStack . active $ set
                fileList    = files . active $ set
                index'      = if (loc == 0) then loc else loc - 1
                browser     = (active set){indexStack=(index':rest)}
            in run $ Profiler set{active=browser} Normal dispatch
        KeyChar 'h'     ->
            changeDir "../" (active set) >>= (\browser -> run $ Profiler set{active=browser} Normal dispatch)
        KeyChar 'l'     ->
            let fileList    = files . active $ set
                index       = head $ indexStack . active $ set
                file        = fileList !! index
            in  if (last file) == '/' then
                    changeDir file (active set) >>= (\browser -> run $ Profiler set{active=browser} Normal dispatch)
                else do
                    spawnFile file dispatch >> (run $ Profiler set Normal dispatch) >> return ()
        KeyChar 'g'     ->
            let (x:xs)      = indexStack . active $ set
                fileList    = files . active $ set
                firstFile   = if (length fileList) < 3 then 0 else 2
                indexStack' = firstFile:xs
                browser     = (active set){indexStack=indexStack'}
            in run $ Profiler set{active=browser} Normal dispatch 
        KeyChar 'G'     ->
            let (x:xs)      = indexStack . active $ set
                lastFile    = (\x -> x - 1) . length . files . active $ set
                indexStack' = lastFile:xs
                browser     = (active set){indexStack=indexStack'}
            in run $ Profiler set{active=browser} Normal dispatch
        KeyChar '^'     ->
            changeDir "~/" (active set) >>= (\browser -> run $ Profiler set{active=browser{indexStack=[0]}} Normal dispatch )
        KeyChar 'q'     -> return ()
        _               -> run $ Profiler set Normal dispatch
handleInput _ _ = return ()

drawBorder :: FileBrowser -> IO ()
drawBorder browser = 
    let w = window browser
    in wAttrSet w (folder, colorYellow) >> wBorder w defaultBorder >> wClearAttribs w

clear :: Profiler -> IO ()
clear (Profiler set _ _) = werase (window (active set)) >> werase (window (passive set))

render :: Profiler -> IO ()
render (Profiler set _ _) =
    drawBorder (active set) >> displayBrowser (active set) >> displayBrowser (passive set)

-- Displays FileBrowser contents
displayBrowser :: FileBrowser -> IO ()
displayBrowser browser = do
    cursSet CursorInvisible
    let w           = window browser
        wholeDir    = files browser 
    wMove w (marginSize - 1) 1
    wAttrSet w (folder, colorYellow) >> wAddStr w (directory browser)
    wClearAttribs w
    wMove w marginSize 0
    showFileList browser >> wRefresh w

-- Calculates indices to display from list
getDisplayListIndices :: FileBrowser -> IO (ScrollIndex Int)
getDisplayListIndices browser = do
    cap <- fileCapacity
    let len         = length $ files browser
        i           = head $ indexStack browser
        threshold   = cap `div` 2
    if (len <= cap) then return $ ScrollIndex 0 len i
    else if (i < threshold) then return $ ScrollIndex 0 cap i
    else if (i < len - threshold) then return $ ScrollIndex (i-threshold) (i+threshold) threshold
    else return $ ScrollIndex (len - cap) len (i + cap - len)

showFileList :: FileBrowser -> IO ()
showFileList browser = 
    let fileList = files browser
    in getDisplayListIndices browser >>= 
        (\(ScrollIndex s e i) -> showList i 0 (sliceList s e fileList))
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
                        let trunc   = truncateFileName limit file
                            file'   = trunc ++ (spaces (limit - (length trunc)))
                        wAttrSet w attrib >> mvWAddStr w (y+1) 1 file' >> wClearAttribs w

-- Generates string of spaces
spaces :: Int -> [Char]
spaces x = take x $ repeat ' '

resize :: IO ()
resize = resizeui >>= (\(y,x) -> resizeTerminal y x) >> erase >> refresh

createLeftWindow :: IO Window
createLeftWindow =
    scrSize >>= (\(y,x) -> newWin (y-marginSize) ((x `div` 2) - marginSize) 0 marginSize)

createRightWindow :: IO Window
createRightWindow =
    scrSize >>= (\(y,x) -> newWin (y-marginSize) ((x `div` 2) - marginSize) 0 (x `div` 2 + marginSize))

initProfiler :: Dispatch -> IO()
initProfiler dispatch = do
    initCurses
    colorsSupported <- colorPairs
    if colorsSupported < 250 then initColors16 else initColors
    cursSet CursorInvisible
    echo False
    w <- initScr
    installHandler (fromJust cursesSigWinch) (Catch resize) Nothing
    wleft <- createLeftWindow
    wRefresh wleft
    wright <- createRightWindow
    wRefresh wright
    update
    dir <- getCurrentDirectory
    list <- sortDirectoryList <$> getDirectoryList dir
    let leftPane    = FileBrowser {window=wleft, directory=dir, files=list, indexStack=[0]}
        rightPane   = FileBrowser {window=wright, directory=dir, files=list, indexStack=[0]}
        set         = WindowSet {active=leftPane, passive=rightPane, orientation=NormalOri}
        profiler    = Profiler set Normal dispatch
    displayBrowser leftPane >> displayBrowser rightPane
    run profiler
    endWin

main = do
    defaultFile <- (++ "/.profiler") <$> getHomeDirectory
    exists <- doesFileExist defaultFile
    if exists then return ()
    else writeFile defaultFile ""
    dispatch <- readConfig defaultFile
    case dispatch of 
        Left d -> initProfiler d
        Right err -> putStrLn err
