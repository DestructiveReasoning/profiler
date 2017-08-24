import ColorManager
import CommanderGeneral
import Data.Maybe
import Dispatch
import Interrogator
import System.Directory (doesFileExist, getCurrentDirectory, getHomeDirectory, setCurrentDirectory)
import System.IO
import System.Posix.Signals
import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper

data ProfilerMode = Normal | Search | Visual | Insert deriving (Show, Eq)

data Orientation = NormalOri | FlippedOri deriving (Eq)

data FeedbackType = ErrorMessage | SuccessMessage | WarningMessage

data WindowSet = WindowSet {
    active      :: FileBrowser,
    passive     :: FileBrowser,
    orientation :: Orientation
}

data Profiler = Profiler WindowSet ProfilerMode Dispatch SearchResult

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

-- Specifies margin on windows
marginSize :: Int
marginSize = 2

-- Returns the total amount of files that can be listed in a window
fileCapacity :: IO Int
fileCapacity = fst <$> scrSize >>= (\y -> pure(y - 3*marginSize))

-- Returns total amount of characters that can be displayed in a window line
fileDisplayLength :: IO Int
fileDisplayLength = snd <$> scrSize >>= (\x -> pure(x `div` 2 - 2*marginSize))


-- Clears formatting and color attributes of window
wClearAttribs :: Window -> IO ()
wClearAttribs window = wAttrSet window (attr0, Pair 0)

run :: Profiler -> IO ()
run profiler = 
    refreshProfiler profiler >>= (\p -> clear p >> render p >> getCh >>= handleInput p)

refreshProfiler :: Profiler -> IO Profiler
refreshProfiler (Profiler set mode disp search) = do
    wl <- createLeftWindow
    wr <- createRightWindow
    win <- createSubWindow
    let act = active set
        pas = passive set
        ori = orientation set
        set' =  if ori == NormalOri then set{active=act{window=wl}, passive=pas{window=wr}}
                else set{active=act{window=wr}, passive=pas{window=wl}}
    werase win
    if mode == Search then
        case search of
            NoResults   -> scrSize >>= (\(y,x) -> mvWAddStr win 0 0 "/")
            Found s ls  -> scrSize >>= (\(y,x) -> mvWAddStr win 0 0 ("/" ++ s))
    else return ()
    wRefresh win
    pure $ Profiler set' mode disp search

reSearch :: Profiler -> Profiler
reSearch (Profiler set mode dispatch search) =
    case search of
        NoResults   -> Profiler set mode dispatch search
        Found x ls  ->
            let fileList    = files . active $ set
                Found s res = getListFromPattern fileList x
            in Profiler set mode dispatch (Found s res)

switchPanes :: Profiler -> IO Profiler
switchPanes (Profiler set Normal dispatch search) = 
    setCurrentDirectory (directory (passive set)) >> (pure $ reSearch $ Profiler (flipWindowSet set) Normal dispatch search)

handleInput :: Profiler -> Key -> IO ()
handleInput (Profiler set Normal dispatch search) input = 
    case input of
        KeyChar '\t'    -> -- Switch frames
            switchPanes (Profiler set Normal dispatch search) >>= run
        KeyChar 'j'     -> -- Move down
            let (loc:rest)  = indexStack . active $ set
                fileList    = files . active $ set
                index'      = if (loc + 1 >= length fileList) then loc else loc + 1
                browser     = (active set){indexStack=(index':rest)}
            in run $ Profiler set{active=browser} Normal dispatch search
        KeyChar 'k'     ->  -- Move up
            let (loc:rest)  = indexStack . active $ set
                fileList    = files . active $ set
                index'      = if (loc == 0) then loc else loc - 1
                browser     = (active set){indexStack=(index':rest)}
            in run $ Profiler set{active=browser} Normal dispatch search
        KeyChar 'h'     -> -- Go up a directory
            changeDir "../" (active set) >>= (\browser -> run $ reSearch (Profiler set{active=browser} Normal dispatch search))
        KeyChar 'l'     -> -- Open directory/file
            let fileList    = files . active $ set
                index       = head $ indexStack . active $ set
                file        = fileList !! index
            in  if (last file) == '/' then
                    changeDir file (active set) >>= (\browser -> run $ reSearch (Profiler set{active=browser} Normal dispatch search))
                else do
                    spawnFile file dispatch >> (run $ Profiler set Normal dispatch search)
        KeyChar 'o'     -> -- Open file with...
            let fileList    = files . active $ set
                index       = head $ indexStack . active $ set
                file        = fileList !! index
                prof        = Profiler set Normal dispatch search
            in 
                if (last file) == '/' then run prof
                else getInput "Open with: " >>= (\p -> openWith (Just p) file dispatch >> run prof)
        KeyChar 'g'     -> -- Go to first file
            let (x:xs)      = indexStack . active $ set
                fileList    = files . active $ set
                firstFile   = if (length fileList) < 3 then 0 else 2
                indexStack' = firstFile:xs
                browser     = (active set){indexStack=indexStack'}
            in run $ Profiler set{active=browser} Normal dispatch search
        KeyChar 'G'     -> -- Go to last file
            let (x:xs)      = indexStack . active $ set
                lastFile    = (\x -> x - 1) . length . files . active $ set
                indexStack' = lastFile:xs
                browser     = (active set){indexStack=indexStack'}
            in run $ Profiler set{active=browser} Normal dispatch search
        KeyChar '^'     -> -- Go to home directory
            changeDir "~/" (active set) >>= (\browser -> run $ reSearch (Profiler set{active=browser{indexStack=[0]}} Normal dispatch search))
        KeyChar '/'     -> -- Search
            run $ Profiler set Search dispatch (Found "" [])
        KeyChar 'n'     -> -- Cycle through search results
            case search of
                NoResults   -> run $ Profiler set Normal dispatch search
                Found s []  -> run $ Profiler set Normal dispatch search
                Found s ls  -> 
                    let (i:is) = indexStack . active $ set
                        i' = cycleSearch Forward i ls
                        indexStack' = i':is
                        browser = (active set){indexStack=indexStack'}
                    in run $ Profiler set{active=browser} Normal dispatch (Found s ls)
        KeyChar 'N'     -> -- Cycle through search results
            case search of
                NoResults   -> run $ Profiler set Normal dispatch search
                Found s []  -> run $ Profiler set Normal dispatch search
                Found s ls  -> 
                    let (i:is) = indexStack . active $ set
                        i' = cycleSearch Backward i ls
                        indexStack' = i':is
                        browser = (active set){indexStack=indexStack'}
                    in run $ Profiler set{active=browser} Normal dispatch (Found s ls)
        KeyChar 'd'     -> do -- Delete file
            let browser = active set
                f       = (files browser) !! (head (indexStack browser))
                prof    = Profiler set Normal dispatch search
            (a:ns) <- getInput ("Delete " ++ f ++ "? (y/n): ")
            if a /= 'Y' && a /= 'y' then run prof
            else do
                result <- deleteFile browser 
                case result of
                    Left err -> giveFeedback ErrorMessage err >> run prof
                    _ -> reindexProfiler (Profiler set Normal dispatch search) >>= run
        KeyChar 'y'     -> do -- Copy file to given destination
            let browser = active set
                f       = (files browser) !! (head (indexStack browser))
                prof    = Profiler set Normal dispatch search
            ans <- getInput ("Copy " ++ f ++ " to: ")
            if (length ans) < 1 then run $ Profiler set Normal dispatch search
            else do
                result <- copyTo ans CP browser
                case result of
                    Left err -> giveFeedback ErrorMessage err >> run prof
                    _ -> reindexProfiler prof >>= run
        KeyChar 'S'     -> do -- Move file to given destination
            let browser = active set
                f       = (files browser) !! (head (indexStack browser))
                prof    = Profiler set Normal dispatch search
            ans <- getInput ("Move " ++ f ++ " to: ")
            if (length ans) < 1 then run prof
            else do
                result <- copyTo ans MV browser 
                case result of
                    Left err -> giveFeedback ErrorMessage err >> run prof
                    _ -> reindexProfiler prof >>= run
        KeyChar 'a'     -> do
            let prof = Profiler set Normal dispatch search
            dir <- getInput "Make directory: " 
            result <- mkdir dir (active set) 
            case result of
                Left err -> giveFeedback ErrorMessage err >> run prof
                _ -> reindexProfiler prof >>= run
        KeyChar 'q'     -> return ()
        _               -> run $ Profiler set Normal dispatch search
handleInput (Profiler set Search dispatch (Found x ls)) input = 
    case input of
        KeyChar '\n'    -> run $ Profiler set Normal dispatch (Found x ls)
        KeyChar '\b'    -> 
            if (length x) == 0 then run $ Profiler set Search dispatch (Found x ls)
            else run $ Profiler set Search dispatch (Found (init x) ls)
        KeyChar '\DEL'    -> 
            if (length x) == 0 then run $ Profiler set Search dispatch (Found x ls)
            else run $ Profiler set Search dispatch (Found (init x) ls)
        KeyBackspace    -> 
            if (length x) == 0 then run $ Profiler set Search dispatch (Found x ls)
            else run $ Profiler set Search dispatch (Found (init x) ls)
        KeyChar c   ->
            let fileList    = files . active $ set
                searchRes   = getListFromPattern fileList (x ++ [c])
            in
                case searchRes of
                NoResults   -> run $ Profiler set Search dispatch (Found (x ++ [c]) ls)
                Found s res -> run $ Profiler (set' res) Search dispatch (Found s res)
            where
                set' r = 
                    let (i:is)  = indexStack . active $ set
                        i'          = if (length r) == 0 then i else if i > (last r) then (head r) else head $ dropWhile (< i) r
                        indexStack' = i':is
                        browser     = (active set){indexStack=indexStack'}
                    in set{active=browser}
        _           -> run $ Profiler set Normal dispatch (Found x [0])

getInput :: String -> IO String
getInput prompt = getProg "" where
    getProg p = do
        win <- createSubWindow
        werase win
        mvWAddStr win 0 0 $ prompt ++ p
        wRefresh win
        c <- getCh
        case c of
            KeyChar '\n'    -> pure p
            KeyChar '\b'    -> 
                let p' = if (length p == 0) then "" else init p
                in getProg $ p'
            KeyChar '\DEL'  ->
                let p' = if (length p == 0) then "" else init p
                in getProg $ p'
            KeyChar c       -> getProg $ p ++ [c]

giveFeedback :: FeedbackType -> String -> IO ()
giveFeedback ErrorMessage msg = do
    w <- createSubWindow
    werase w >> wAttrSet w (folder, colorError)
    mvWAddStr w 0 0 msg >> mvWAddStr w 1 0 "Press any key to continue..." >> wClearAttribs w >> wRefresh w
    getCh >> return ()
giveFeedback _ _ = return ()

drawBorder :: FileBrowser -> IO ()
drawBorder browser = 
    let w = window browser
    in wAttrSet w (folder, colorBorder) >> wBorder w defaultBorder >> wClearAttribs w

clear :: Profiler -> IO ()
clear (Profiler set _ _ _) = werase (window (active set)) >> werase (window (passive set))

render :: Profiler -> IO ()
render (Profiler set mode _ _) =
    drawBorder (active set) >> displayBrowser (active set) >> displayBrowser (passive set)

-- Displays FileBrowser contents
displayBrowser :: FileBrowser -> IO ()
displayBrowser browser = do
    cursSet CursorInvisible
    (y, x) <- scrSize
    let w           = window browser
        limit       = x `div` 2 - 2 * marginSize
        wholeDir    = files browser 
        cwd         = directory browser
        truncDir    = if (length cwd) < limit then cwd else drop ((length cwd) - limit) cwd
    wMove w (marginSize - 1) 1
    wAttrSet w (folder, colorCWD) >> wAddStr w truncDir
    wClearAttribs w
    wMove w marginSize 0
    showFileList browser >> wRefresh w

reindexProfiler :: Profiler -> IO Profiler
reindexProfiler (Profiler set m d s) = do
    a' <- reindexBrowser $ active set
    p' <- reindexBrowser $ passive set
    pure $ Profiler set{active=a', passive=p'} m d s

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
                    in do
                        attrib <- getAttrib isSelected file
                        (y,_) <- getYX w
                        limit <- fileDisplayLength
                        let trunc   = truncateFileName limit file
                            file'   = if (isSelected) then trunc ++ (spaces (limit - (length trunc))) else trunc
                        wAttrSet w attrib >> mvWAddStr w (y+1) 1 file' >> wClearAttribs w
            getAttrib isSelected file = 
                if isSelected then pure (selected, colorSelected)
                else do
                    let isFolder = (last file) == '/'
                        style = if isFolder then folder else attr0
                    exec <- isExecutable file
                    link <- isSymLink file
                    let color = if exec == Right True then colorExec
                                else if link  == Right True then colorLink 
                                else if isFolder then colorFolder
                                else (Pair 0)
                    pure (style, color)

-- Generates string of spaces
spaces :: Int -> [Char]
spaces x = take x $ repeat ' '

resize :: IO ()
resize = resizeui >>= (\(y,x) -> resizeTerminal y x)

createLeftWindow :: IO Window
createLeftWindow =
    scrSize >>= (\(y,x) -> newWin (y-marginSize) ((x `div` 2) - marginSize) 0 marginSize)

createRightWindow :: IO Window
createRightWindow =
    scrSize >>= (\(y,x) -> newWin (y-marginSize) ((x `div` 2) - marginSize) 0 (x `div` 2 + marginSize))

createMainWindow :: IO Window
createMainWindow = 
    scrSize >>= (\(y,x) -> newWin y x 0 0)

createSubWindow :: IO Window
createSubWindow =
    scrSize >>= (\(y,x) -> newWin marginSize x (y - marginSize) 0)

initProfiler :: Dispatch -> IO()
initProfiler dispatch = do
    initCurses
    colorsSupported <- colorPairs
    cursSet CursorInvisible
    echo False
    installHandler (fromJust cursesSigWinch) (Catch resize) Nothing
    if colorsSupported < 250 then initColors16 
    else do
        defaultColorFile <- (++ "/.profiler/colors") <$> getHomeDirectory
        res <- colorConfig defaultColorFile
        case res of
            Left _ -> do
                w <- initScr
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
                    profiler    = Profiler set Normal dispatch NoResults
                displayBrowser leftPane >> displayBrowser rightPane
                run profiler
                endWin
            Right err -> endWin >> putStrLn err

main = do
    defaultFile <- (++ "/.profiler/defaults") <$> getHomeDirectory
    exists <- doesFileExist defaultFile
    if exists then return ()
    else writeFile defaultFile ""
    dispatch <- readConfig defaultFile
    case dispatch of 
        Left d -> initProfiler d
        Right err -> putStrLn err
