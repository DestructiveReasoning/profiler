import CommanderGeneral
import System.Directory (getCurrentDirectory)
import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper

data ProfilerMode = Normal | Search | Visual | Insert deriving (Show, Eq)

data FileMod = MV | CP | MKDIR deriving (Eq, Show)

--data Activated = LeftBrowser | RightBrowser

data Profiler = Profiler FileBrowser FileBrowser ProfilerMode

-- Stores list scroll data.
-- First parameter: index to start displaying list
-- Second parameter: index to stop displaying list (exclusive)
-- Third parameter: index within modified list that is selected
data ScrollIndex a = ScrollIndex a a a

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

run :: Profiler -> IO ()
run profiler =
    clear profiler >> render profiler >> getCh >>= handleInput profiler

handleInput :: Profiler -> Key -> IO ()
handleInput (Profiler active passive Normal) input = 
    case input of
        KeyChar '\t'    -> run $ Profiler passive active Normal
        KeyChar 'j'     -> 
            let (loc:rest)  = indexStack active
                fileList    = files active
                index'      = if (loc + 1 >= length fileList) then loc else loc + 1
            in run $ Profiler active{indexStack=(index':rest)} passive Normal
        KeyChar 'k'     -> 
            let (loc:rest)  = indexStack active
                fileList    = files active
                index'      = if (loc == 0) then loc else loc - 1
            in run $ Profiler active{indexStack=(index':rest)} passive Normal
        KeyChar 'h'     ->
            changeDir "../" active >>= (\browser -> run $ Profiler browser passive Normal)
        KeyChar 'l'     ->
            let file = (files active) !! (head (indexStack active))
            in  if (last file) == '/' then
                changeDir file active >>= (\browser -> run $ Profiler browser passive Normal)
                else run $ Profiler active passive Normal -- TODO Implement opening files
        KeyChar 'g'     ->
            let (x:xs)      = indexStack active
                fileList    = files active
                firstFile   = if (length fileList) < 3 then 0 else 2
                indexStack' = firstFile:xs
            in run $ Profiler (active{indexStack=indexStack'}) passive Normal
        KeyChar 'G'     ->
            let (x:xs)      = indexStack active
                lastFile    = (\x -> x - 1) . length . files $ active
                indexStack' = lastFile:xs
            in run $ Profiler (active{indexStack=indexStack'}) passive Normal
        KeyChar '^'     ->
            changeDir "~/" active >>= (\browser -> run $ Profiler browser passive Normal)
        KeyChar 'q'     -> return ()
        _               -> run $ Profiler active passive Normal
handleInput _ _ = return ()

drawBorder :: FileBrowser -> IO ()
drawBorder browser = 
    let w = window browser
    in wAttrSet w (folder, colorYellow) >> wBorder w defaultBorder >> wClearAttribs w

clear :: Profiler -> IO ()
clear (Profiler active passive _) = wclear (window active) >> wclear (window passive)

render :: Profiler -> IO ()
render (Profiler active passive _) =
    drawBorder active >> displayBrowser active >> displayBrowser passive

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
                            file'   = trunc ++ (spaces (limit - (length trunc) - marginSize))
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
        profiler    = Profiler leftPane rightPane Normal
    displayBrowser leftPane >> displayBrowser rightPane
    run profiler
    endWin
