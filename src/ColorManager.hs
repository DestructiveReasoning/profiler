module ColorManager 
where

import Data.Maybe
import System.Directory
import System.IO
import Text.Read
import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper

data ColorSetting = ColorSetting Int Int Int

data ErrorType = InvalidClass | BadFormat | BadColor | InvalidColor

colorConfig :: FilePath -> IO (Either () String)
colorConfig file = 
    doesFileExist file >>= (\b -> if b then parseColorConfig file else initColors >> pure (Left ()))

readColorConfig :: FilePath -> IO (Either [ColorSetting] String)
readColorConfig file = 
    (parseLines [] 1) . lines <$> (readFile file)
    where 
    getPair str = 
        case str of
            "selected"      -> Just 1
            "folder"        -> Just 2
            "executable"    -> Just 3
            "border"        -> Just 4
            "cwd"           -> Just 5
            "error"         -> Just 6
            "symlink"       -> Just 7
            _ -> Nothing
    parseLines cfg _ [] = Left cfg
    parseLines cfg n (x:xs) =
        let w = words x
            p = getPair (w !! 0)
        in
        if (length w) /= 3 then Right $ showSyntaxError BadFormat n x
        else if isNothing p then Right $ showSyntaxError InvalidClass n (w !! 0)
        else
            let resf = readMaybe (w !! 1) :: Maybe Int
                resb = readMaybe (w !! 2) :: Maybe Int
            in case (resf,resb) of
                (Nothing,_) -> Right $ showSyntaxError BadColor n (w !! 1)
                (_,Nothing) -> Right $ showSyntaxError BadColor n (w !! 2)
                (Just f,Just b) -> 
                    if f > 255 || b > 255  || f < -1 || b < -1 then
                        Right $ showSyntaxError InvalidColor n x
                    else parseLines (cfg ++ [(ColorSetting (fromJust p) f b)]) (n+1) xs

parseColorConfig :: FilePath -> IO (Either () String)
parseColorConfig file = do
    cfg <- readColorConfig file
    case cfg of
        Left settings -> genColors settings
        Right err     -> pure $ Right err
    where
    genColors [] = pure $ Left ()
    genColors (x:xs) = 
        let (ColorSetting pair fg bg) = x
            fg' = if fg == -1 then defaultForeground else Color fg
            bg' = if bg == -1 then defaultBackground else Color bg
        in initPair (Pair pair) fg' bg' >> (genColors xs)

showSyntaxError :: ErrorType -> Int -> String -> String
showSyntaxError t num err = 
    let prefix = "[Color Settings] Syntax error on line " ++ (show num) ++ " - "
    in
    case t of
        InvalidClass ->  prefix ++ "Invalid color class: \"" ++ err ++ "\"."
        BadFormat -> 
            prefix ++ "Invalid format. Color settings should contain 3 columns, but " ++ (show (length (words err))) ++ " are given."
        BadColor -> prefix ++ "Invalid color setting: \"" ++ err ++ "\". Color must be an integer."
        InvalidColor -> prefix ++ "Invalid color setting: \"" ++ err ++ "\". Color must be between -1 and 255."

-- Legend 
-- Pair 1:  selected color
-- Pair 2:  folder color
-- Pair 3:  executable color
-- Pair 4:  border color
-- Pair 5:  cwd color
-- Pair 6:  error color

-- ATTRIBUTES
selected = convertAttributes [Reverse, Bold]
folder = convertAttributes [Bold]

-- COLORS
initColors = do
    initPair (Pair 1) (Color 60) defaultBackground
    initPair (Pair 2) blue defaultBackground
    initPair (Pair 3) (Color 185) defaultBackground
    initPair (Pair 4) (Color 220) defaultBackground
    initPair (Pair 5) (Color 220) defaultBackground
    initPair (Pair 6) (Color 124) defaultBackground
    initPair (Pair 7) (Color 63) defaultBackground

initColors16 = do
    initPair (Pair 1) blue defaultBackground
    initPair (Pair 2) blue defaultBackground
    initPair (Pair 3) magenta defaultBackground
    initPair (Pair 4) yellow defaultBackground
    initPair (Pair 5) green defaultBackground

colorSelected = Pair 1
colorFolder = Pair 2
colorExec = Pair 3
colorBorder = Pair 4
colorCWD = Pair 5
colorError = Pair 6
colorLink = Pair 7
