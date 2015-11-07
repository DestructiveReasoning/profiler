module Dispatch
( 
 dispatch
,procedures
,isCodeFile
,code
) where

dispatch = [(".pdf",["evince","mupdf"]),
            (".png",["feh","gimp"]),
            (".jpg",["feh","gimp"]),
            (".xcf",["gimp"]),
            (".docx",["libreoffice"]),
            (".doc",["libreoffice"]),
            (".xls",["libreoffice"]),
            (".xlsx",["libreoffice"]),
            (".ppt",["libreoffice"]),
            (".pptx",["libreoffice"]),
            (".dvi",["evince"]),
            (".torrent",["deluge","vuze","utorrent"]),
            (".gp3",["tuxguitar"]),
            (".gp4",["tuxguitar"]),
            (".gp5",["tuxguitar"]),
            (".gp6",["tuxguitar"]),
            ("code",["vim","subl","gedit","emacs","nano"]),
            (".epub",["cr3"])]

code = [".hs",".c",".cpp",".h",".java",".hpp",".py",".js",".cs",".sh",".txt",".tex"]

isCodeFile :: String -> Bool
isCodeFile ext = ext `elem` code

--procedures :: String -> Maybe [String]
--procedures ext = if [n | (x,n) <- dispatch, x == ext] /= [] then Just (head [n | (x,n) <- dispatch, x == ext]) else Nothing
procedures :: String -> [String]
procedures ext = head [n | (x,n) <- dispatch, x == ext]
