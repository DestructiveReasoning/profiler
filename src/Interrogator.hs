module Interrogator
(
     findConcretePattern
    ,getListFromPattern
    ,SearchResult (..)
) where

import Data.Maybe
import Text.Regex

data SearchResult = NoResults | Found String [Int]

-- Get list of indices in file list that matches a pattern
getListFromPattern :: [String] -> String -> SearchResult
getListFromPattern list pattern = 
    let regex = mkRegex $ ".*" ++ pattern ++ ".*"
    in Found pattern [x | x <- [0..(length list - 1)], isJust $ matchRegex regex (list !! x)]

-- Get first index of list item that matches a prefix
findConcretePattern :: [String] -> String -> Int
findConcretePattern list pattern = 
    length $ takeWhile (/= pattern) $ map (take (length pattern)) list
