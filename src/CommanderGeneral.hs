module CommanderGeneral
(   
    cd
  , changeDir
  , deleteFile
  , fileChar
  , getDirectoryList
  , quicksort
  , makeProperDirectory
  , truncateFileName
  , sortDirectoryList
  , sliceList
  , FileBrowser (..)
) where

import Control.Monad
import Data.Char
import Data.List.Split
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Process
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

truncateFileName :: Int -> FilePath -> FilePath
truncateFileName limit file =
    if length file <= limit then file
    else if last file == '/' then (take (limit - 3) file) ++ "~~/"
    else
        let parts = splitOn "." file
            extension = if (length parts) <= 1 || length (head parts) == 0 then "" else (last parts)
            extension' =    if (length extension) == 0 then ""
                            else if ((length extension) + 4) < limit then '.':extension
                            else '.':[head extension]
            rem = limit - (length extension')
        in (take (rem - 3) file) ++ "~~~" ++ extension'

-- Format directory to be understood by BASH
makeProperDirectory :: FilePath -> FilePath
makeProperDirectory [] = []
makeProperDirectory (x:xs) = 
    let badChars = " \'[]()"
    in if x `elem` badChars then "\\" ++ [x] ++ makeProperDirectory xs 
                            else x:(makeProperDirectory xs)

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

deleteFile :: FileBrowser -> IO FileBrowser
deleteFile browser = 
    let f       = (files browser) !! (head (indexStack browser))
        (i:is)  = indexStack browser
        i'      = if i == (length (files browser)) - 1 then i - 1 else i
        dir     = directory browser
    in
        if (last f == '/') then return browser
        else
            (\x -> x ++ "/" ++ f) <$> getCurrentDirectory >>= removeFile >>
            sortDirectoryList <$> getDirectoryList dir >>= (\l -> return browser{files=l, indexStack=(i':is)})
