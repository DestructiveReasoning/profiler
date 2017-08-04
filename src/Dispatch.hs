module Dispatch
( 
 isTextFile
,spawnFile
,readConfig
,text
,Dispatch
) where

import Data.List.Split
import Data.Maybe
import System.Directory
import System.IO
import System.Process

type Dispatch = [(String, [String])]

spawnFile :: FilePath -> Dispatch -> IO ()
spawnFile file dispatch = do
    writeFile "debug" ("Opening " ++ file ++ " (extension " ++ ((last . splitOn ".") file) ++ ") with ")
    let prog = getLauncher file dispatch
    case prog of 
        Nothing -> appendFile "debug" "... nevermind"
        Just p -> do
            appendFile "debug" p
            createProcess (proc (p ++ file) []) >> return ()

getLauncher :: FilePath -> Dispatch -> Maybe String
getLauncher file dispatch= 
    let ext = last . splitOn "." $ file
    in findProg ext dispatch
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
            ("text",["vim","subl","gedit","emacs","nano"]),
            ("epub",["cr3"])]

readConfig :: FilePath -> IO (Either Dispatch String)
readConfig file =
    (((parseLines defaultApps 1). lines) <$> (readFile file))

parseLines :: Dispatch -> Int -> [String] -> Either Dispatch String
parseLines dispatch _ [] = Left dispatch
parseLines dispatch num (x:xs) =
    let parsed = words x
    in  if (length parsed) /= 2 then Right $ "Syntax error on line " ++ (show num) ++ ":\n" ++ x
        else parseLines (modifyDispatch dispatch (parsed !! 0) (parsed !! 1)) (num + 1) xs

modifyDispatch :: Dispatch -> String -> String -> Dispatch
modifyDispatch [] ext prog = [(ext,[prog])]
modifyDispatch (x:xs) ext prog = 
    if ext == (fst x) then (ext, prog:(snd x)):xs
    else modifyDispatch xs ext prog

text = [".hs",".c",".cpp",".h",".java",".hpp",".py",".js",".cs",".sh",".txt",".tex"]

isTextFile :: String -> Bool
isTextFile ext = ext `elem` text
