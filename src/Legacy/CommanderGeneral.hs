module CommanderGeneral
(   ls
  , cd
  , getDirectoryList
  , sortDirectoryList
  , removeCommons
  , formatDirectory
  , truncateFile
  , lastChar
  , findPattern
  , getListFromPattern
  , fileChar
  , makeProperDirectory
) where

import Control.Monad
import System.Directory
import System.Exit
import System.IO
import System.Process
import Data.Char

fileChar = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'] ++ ['1'..'9'] ++ "!@#$%^&*()_+~`^-={}/?\"\';:<>,. "

data Command = PROMPT|EXIT deriving (Eq)

commandEq :: Command -> Command -> Bool
commandEq PROMPT PROMPT = True
commandEq EXIT EXIT = True
commandEq _ _ = False

prompt :: String -> IO() 
prompt s = do
    putStr s
    hFlush stdout

getListFromPattern :: [String] -> String -> [Int]
getListFromPattern list pattern =
    let sufs = map (take (length pattern)) list
    in [x | x <- [0,1..(length list - 1)], pattern == sufs !! x]

findPattern :: [String] -> String -> Int
findPattern list pattern = 
    let len = length pattern
        in length $ takeWhile(/= pattern) $ map (take len) list
--        in foldl (\acc x -> if ((take len) x) == pattern then acc else acc + 1) 0 list                          

lastChar :: [Char] -> Char
lastChar [] = ' '
lastChar xs = head $ (dropWhile (== ' ') . reverse) xs

truncateFile :: Int -> FilePath -> FilePath
truncateFile len file = if (length file <= len) then file
                        else if (lastChar file == '/') then (take (len - 3) file) ++ "~~/"
                        else (take (len - 3) file) ++ "..."

formatDirectory :: Char -> String -> FilePath -> FilePath
formatDirectory _ _ [] = []
formatDirectory x y (s:ss) = if s == x then (y ++ (formatDirectory x y ss)) else s:(formatDirectory x y ss)

makeProperDirectory :: FilePath -> FilePath
makeProperDirectory [] = []
makeProperDirectory (x:xs) =
    if x == ' ' then ['\\'] ++ [' ']  ++ (makeProperDirectory xs)
    else if x == '\'' then "\\'" ++ (makeProperDirectory xs)
    else if x == '[' then "\\[" ++ (makeProperDirectory xs)
    else if x == ']' then "\\]" ++ (makeProperDirectory xs)
    else [x] ++ (makeProperDirectory xs)

--makeProperDirectory :: FilePath -> FilePath
--makeProperDirectory xs = 
--    if '\'' `elem` xs || ' ' `elem` xs then ['\"'] ++ xs ++ ['\"']
--    else xs

ls :: FilePath -> IO()
ls path = do
    list <- getDirectoryList path
    mapM_ putStrLn list

cd :: FilePath -> IO()
cd "" = do
    home <- getHomeDirectory
    setCurrentDirectory home
    return()
cd (d1:ds) 
    | d1 == '/' = do
        setCurrentDirectory (d1:ds)
        return ()
    | d1 == '~' = do
        home <- getHomeDirectory
        setCurrentDirectory $ home ++ "/" ++ ds
        return ()
    | otherwise = do
        path <- getCurrentDirectory
        setCurrentDirectory $ path ++ "/" ++ (d1:ds)
        return ()

removeCommons :: (Eq a) => [a] -> [a] -> [a]
removeCommons [] _ = []
removeCommons l [] = l
removeCommons (x:xs) (ys) =
    if x `elem` ys then removeCommons xs ys
    else x:(removeCommons xs ys)

sortDirectoryList :: [FilePath] -> [FilePath]
sortDirectoryList [] = []
sortDirectoryList list = 
    let dirs    = sort $ filter (elem '/') list
        files   = sort $ removeCommons list dirs
        in dirs ++ files
        where
            sort [] = []
            sort (x:xs) = 
                let smaller = sort $ filter (< x) xs
                    greater = sort $ removeCommons xs smaller
                    in smaller ++ [x] ++ greater

getDirectoryList :: FilePath -> IO [FilePath]
getDirectoryList path = do
    contents <- getDirectoryContents path
    bools <- mapM (doesDirectoryExist) contents
    let list = zipWith (\dir file -> if dir then file ++ "/" else file) bools contents
    return list

end :: IO()
end = do
    cmdLine "/" EXIT

cmdLine :: FilePath -> Command -> IO()
cmdLine _ EXIT = do
    putStrLn "Goodbye"
    exitWith ExitSuccess
cmdLine path command = do
    prompt $ "profiler - " ++ path ++ "> "
    cmd <- getLine
    case cmd of "ls" -> ls path
                ('c':'d':rest) -> cd $ rest
                "exit" -> end
                "clear" -> callCommand "clear"
                _ -> putStrLn "Invalid input"
--    return()
    newpath <- getCurrentDirectory
    if (commandEq command PROMPT) then cmdLine newpath PROMPT else exitWith ExitSuccess

main = do
    home <- getHomeDirectory
    dir <- getCurrentDirectory
    cmdLine dir PROMPT
