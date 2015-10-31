module CommanderGeneral
( commandEq
  , prompt
  , ls
  , cd
  , getDirectoryList
) where

import System.IO
import System.Directory
import Control.Monad
import System.Exit
import System.Process


data Command = PROMPT|EXIT deriving (Eq)

commandEq :: Command -> Command -> Bool
commandEq PROMPT PROMPT = True
commandEq EXIT EXIT = True
commandEq _ _ = False

prompt :: String -> IO() 
prompt s = do
    putStr s
    hFlush stdout

ls :: FilePath -> IO()
ls path = do
    list <- getDirectoryList path
    mapM_ putStrLn list

cd :: FilePath -> IO()
cd "" = do
    home <- getHomeDirectory
    setCurrentDirectory home
    return()
cd (d:d1:ds) 
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
