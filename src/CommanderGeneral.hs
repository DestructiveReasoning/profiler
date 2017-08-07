module CommanderGeneral
(   
    cd
  , changeDir
  , fileChar
  , findConcretePattern
  , getDirectoryList
  , getListFromPattern
  , quicksort
  , makeProperDirectory
  , truncateFileName
  , sortDirectoryList
  , sliceList
  , FileBrowser (..)
) where

import Control.Monad
import Data.Char
import Data.Maybe
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Process
import Text.Regex
import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper

data FileBrowser = FileBrowser {
    window :: Window,
    directory :: FilePath,
    files :: [FilePath],
    indexStack :: [Int]
} deriving (Show)

fileChar = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'] ++ ['1'..'9'] ++ "!@#$%^&*()_+~`^-={}/?\"\';:<>,. "

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smaller = quicksort $ filter (< x) xs
        greater = quicksort $ filter (>= x) xs
        in smaller ++ [x] ++ greater

-- Get list of indices in file list that matches a pattern
getListFromPattern :: [String] -> String -> [Int]
getListFromPattern list pattern = 
    let regex = mkRegex $ ".*" ++ pattern ++ ".*"
    in [x | x <- [0..(length list - 1)], isJust $ matchRegex regex (list !! x)]

-- Get first index of list item that matches a prefix
findConcretePattern :: [String] -> String -> Int
findConcretePattern list pattern = 
    length $ takeWhile (/= pattern) $ map (take (length pattern)) list

truncateFileName :: Int -> FilePath -> FilePath
truncateFileName limit file =
    if length file <= limit then file
    else if last file == '/' then (take (limit - 3) file) ++ "~~/"
    else (take (limit - 3) file) ++ "~~~"

-- Format directory to be understood by BASH
makeProperDirectory :: FilePath -> FilePath
makeProperDirectory [] = []
makeProperDirectory (x:xs) = 
    case x of
        ' '     -> "\\ " ++ (makeProperDirectory xs)
        '\''    -> "\\'" ++ (makeProperDirectory xs)
        '['     -> "\\[" ++ (makeProperDirectory xs)
        ']'     -> "\\]" ++ (makeProperDirectory xs)
        _       -> x:(makeProperDirectory xs)

-- Get list of files within directory
getDirectoryList :: FilePath -> IO [FilePath]
getDirectoryList path = 
    getDirectoryContents path >>= mapM modDir
    where
        modDir path = (\isDir -> if isDir then path ++ "/" else path) <$> doesDirectoryExist path

-- Sort directory list, placing all directories above regular files
sortDirectoryList :: [FilePath] -> [FilePath]
sortDirectoryList [] = []
sortDirectoryList list = 
    let dirs    = quicksort $ filter (\x -> last x == '/') list
        files   = quicksort $ filter (\x -> last x /= '/') list
        in dirs ++ files

-- Get portion of list between two indices
sliceList :: Int -> Int -> [a] -> [a]
sliceList start end = (drop start) . (take end)

-- Change working directory
cd :: FilePath -> IO ()
cd "" = getHomeDirectory >>= setCurrentDirectory
cd (prefix:rest)
    | prefix == '/' = setCurrentDirectory (prefix:rest)
    | prefix == '~' = getHomeDirectory >>= setCurrentDirectory . (++ rest)
    | otherwise     = getCurrentDirectory >>= setCurrentDirectory . (</> (prefix:rest))

-- Change directory of FileBrowser
changeDir :: FilePath -> FileBrowser -> IO FileBrowser
changeDir path browser = 
    do  setCurrentDirectory $ directory browser
        cd path
        directory' <- getCurrentDirectory
        files' <- sortDirectoryList <$> getDirectoryList directory'
        let stack = indexStack browser
            indexStack' = 
                if path == "../" then 
                    if (length stack) < 2 then [0]
                    else tail stack 
                else 0:stack
            in return browser{ directory = directory', files = files', indexStack = indexStack' }
