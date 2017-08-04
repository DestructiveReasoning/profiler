module Dispatch
( 
 procedures
,isTextFile
,readConfig
,text
,Dispatch
) where

import Data.Maybe
import System.Directory
import System.IO

type Dispatch = [(String, [String])]

defaultApps :: Dispatch
defaultApps = [(".pdf",["evince","firefox","mupdf"]),
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
    (((parseLines defaultApps). lines) <$> (readFile file))

parseLines :: Dispatch -> [String] -> Either Dispatch String
parseLines dispatch [] = Left dispatch
parseLines dispatch (x:xs) =
    let parsed = words x
    in  if (length parsed) /= 2 then Right $ "Syntax error on line:\n" ++ x
        else parseLines (modifyDispatch dispatch (parsed !! 0) (parsed !! 1)) xs

modifyDispatch :: Dispatch -> String -> String -> Dispatch
modifyDispatch [] ext prog = [(ext,[prog])]
modifyDispatch (x:xs) ext prog = 
    if ext == (fst x) then (ext, prog:(snd x)):xs
    else modifyDispatch xs ext prog

text = [".hs",".c",".cpp",".h",".java",".hpp",".py",".js",".cs",".sh",".txt",".tex"]

isTextFile :: String -> Bool
isTextFile ext = ext `elem` text

--procedures :: String -> Maybe [String]
--procedures ext = if [n | (x,n) <- dispatch, x == ext] /= [] then Just (head [n | (x,n) <- dispatch, x == ext]) else Nothing
procedures :: String -> [String]
procedures ext = head [n | (x,n) <- defaultApps, x == ext]
