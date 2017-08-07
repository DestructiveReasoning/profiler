module Dispatch
( 
 isTextFile
,openWith
,spawnFile
,readConfig
,text
,Dispatch
) where

import CommanderGeneral
import Data.List.Split
import Data.Maybe
import System.Directory
import System.IO
import System.Posix.Process
import System.Process

type Dispatch = [(String, [String])]

spawnFile :: FilePath -> Dispatch -> IO ()
spawnFile file dispatch = openWith (getLauncher file dispatch) file dispatch

openWith :: Maybe String -> FilePath -> Dispatch -> IO ()
openWith prog file dispatch = 
    let file' = makeProperDirectory file
    in case prog of 
        Nothing -> return ()
        Just p -> createProcess (shell (p ++ " " ++ file')) { std_out = CreatePipe, std_err = CreatePipe, new_session = True } >> return ()

getLauncher :: FilePath -> Dispatch -> Maybe String
getLauncher file dispatch = 
    let ext = splitOn "." . dropWhile (== '.') $  file --Only consider filename after leading dots to allow interpretation of hidden files
    in if (length ext) < 2 then findProg "text" dispatch else findProg (last ext) dispatch
    where
        findProg _ [] = Nothing
        findProg extension (x:xs) = 
            if extension == (fst x) then Just ((head . snd) x)
            else findProg extension xs

defaultApps :: Dispatch
defaultApps = [("pdf",["evince","firefox","mupdf"]),
            ("png",["feh","gimp"]),
            ("jpg",["feh","gimp"]),
            ("xcf",["gimp"]),
            ("docx",["libreoffice"]),
            ("doc",["libreoffice"]),
            ("xls",["libreoffice"]),
            ("xlsx",["libreoffice"]),
            ("ppt",["libreoffice"]),
            ("pptx",["libreoffice"]),
            ("dvi",["evince"]),
            ("torrent",["deluge","vuze","utorrent"]),
            ("gp3",["tuxguitar"]),
            ("gp4",["tuxguitar"]),
            ("gp5",["tuxguitar"]),
            ("gp6",["tuxguitar"]),
            ("text",["xterm -e vim","subl","gedit","xterm -e emacs","xterm -e nano"]),
            ("epub",["cr3"])]

readConfig :: FilePath -> IO (Either Dispatch String)
readConfig file =
    (((parseLines defaultApps 1) . lines) <$> (readFile file))

parseLines :: Dispatch -> Int -> [String] -> Either Dispatch String
parseLines dispatch _ [] = Left dispatch
parseLines dispatch num (x:xs) =
    let parsed = map (dropWhile (== ' ')) $ splitOn "," x
    in  if (length parsed) /= 2 then Right $ "Syntax error on line " ++ (show num) ++ ":\n" ++ x
        else parseLines (modifyDispatch dispatch (parsed !! 0) (parsed !! 1)) (num + 1) xs

modifyDispatch :: Dispatch -> String -> String -> Dispatch
modifyDispatch [] ext prog = [(ext,[prog])]
modifyDispatch (x:xs) ext prog = 
    if ext == (fst x) then (ext, prog:(snd x)):xs
    else x:(modifyDispatch xs ext prog)

text = [".hs",".c",".cpp",".h",".java",".hpp",".py",".js",".cs",".sh",".txt",".tex"]

isTextFile :: String -> Bool
isTextFile ext = ext `elem` text
