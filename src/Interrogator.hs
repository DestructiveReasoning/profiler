module Interrogator
(
     cycleSearch
    ,findConcretePattern
    ,getListFromPattern
    ,validateRegex
    ,CycleDir (..)
    ,SearchResult (..)
) where

import Data.Maybe
import Text.Regex

data SearchResult = NoResults | Found String [Int]

data CycleDir = Forward | Backward deriving (Eq)

-- Get list of indices in file list that matches a pattern
getListFromPattern :: [String] -> String -> SearchResult
getListFromPattern list pattern = 
    if (validateRegex pattern) then
        let regex = mkRegex $ ".*" ++ pattern ++ ".*"
        in Found pattern [x | x <- [0..(length list - 1)], isJust $ matchRegex regex (list !! x)]
    else NoResults

cycleSearch :: CycleDir -> Int -> [Int] -> Int
cycleSearch Forward index is =
    if (index + 1) > (last is) then head is
    else head $ dropWhile (< index + 1) is
cycleSearch Backward index is =
    if (index - 1) < (head is) then last is
    else last $ takeWhile (<= index - 1) is

validateRegex :: String -> Bool
validateRegex = validateParens

validateParens :: String -> Bool
validateParens pattern = 
    iterate pattern (0,0,0)
    where
        iterate [] (a,b,c) = (a == 0) && (b == a) && (c == b)
        iterate (x:xs) (a,b,c) = 
            case x of
            '(' -> iterate xs (a + 1, b, c)
            ')' -> iterate xs (a - 1, b, c)
            '[' -> iterate xs (a, b + 1, c)
            ']' -> iterate xs (a, b - 1, c)
            '{' -> iterate xs (a, b, c + 1)
            '}' -> iterate xs (a, b, c - 1)
            _   -> iterate xs (a, b, c)

-- Get first index of list item that matches a prefix
findConcretePattern :: [String] -> String -> Int
findConcretePattern list pattern = 
    length $ takeWhile (/= pattern) $ map (take (length pattern)) list
