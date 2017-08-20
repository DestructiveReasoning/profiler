module CommanderGeneral
(   
    cd
  , changeDir
  , copyTo
  , deleteFile
  , fileChar
  , getDirectoryList
  , isExecutable
  , quicksort
  , makeProperDirectory
  , mkdir
  , truncateFileName
  , reindexBrowser
  , sortDirectoryList
  , sliceList
  , FileBrowser (..)
  , FileMod (..)
) where

import Control.Exception
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

data FileMod = CP | MV deriving (Eq)

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
    setCurrentDirectory path >> getDirectoryContents path >>= mapM modDir
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
                else if path == "./" then stack
                else 0:stack
            in return browser{ directory = directory', files = files', indexStack = indexStack' }

reindexBrowser :: FileBrowser -> IO FileBrowser
reindexBrowser browser = 
    let (i:is)  = indexStack browser
        maxi    = (length . files) browser - 1
        i'      = if i > maxi then maxi else i
        dir     = directory browser
    in sortDirectoryList <$> getDirectoryList dir >>= (\l -> return browser{files=l, indexStack=((min i' ((length l)-1)):is)})
    where min a b = if a < b then a else b

deleteFile :: FileBrowser -> IO (Either String ())
deleteFile browser = 
    let f = (files browser) !! (head (indexStack browser))
    in  if (last f == '/') then return (Left "Cannot delete directory")
        else
            setCurrentDirectory (directory browser) >>
            (\x -> x ++ "/" ++ f) <$> getCurrentDirectory >>= removeFile'
    where removeFile' f = do
            result <- try (removeFile f) :: IO (Either SomeException ())
            case result of
                Left ex -> return (Left "Cannot delete file")
                Right _ -> return (Right ())

copyTo :: FilePath -> FileMod -> FileBrowser -> IO (Either String ())
copyTo destination op browser = 
    let f = (files browser) !! (head (indexStack browser))
    in  if (last f == '/') || (length destination == 0) then return (Left "Invalid source or destination")
        else do
            setCurrentDirectory $ directory browser
            src <- (\x -> x ++ "/" ++ f) <$> getCurrentDirectory
            dst <- (\d -> if d then destination ++ "/" ++ f else destination) <$> doesDirectoryExist destination
            renameFile' src dst
    where renameFile' a b = do
            result <- try (if op == CP then copyFile a b else renameFile a b) :: IO (Either SomeException ())
            case result of
                Left ex -> return (Left "Cannot copy/move file")
                Right _ -> return (Right ())

mkdir :: FilePath -> FileBrowser -> IO (Either String ())
mkdir dir browser = setCurrentDirectory (directory browser) >> createDirectoryIfMissing' dir
    where createDirectoryIfMissing' a = do
            result <- try (createDirectoryIfMissing True a) :: IO (Either SomeException ())
            case result of
                Left ex -> return (Left "Cannot make directory with that name")
                Right _ -> return (Right ())

isExecutable :: FilePath -> IO (Either String Bool)
isExecutable path = do
    result <- try (executable <$> getPermissions path) :: IO (Either SomeException Bool)
    case result of
        Left ex -> return (Left "Insufficient power to view permissions")
        Right b -> return (Right b)
