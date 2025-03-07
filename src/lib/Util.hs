module Lib.Util where

import Data.Char (isSpace)
import Data.List.Split (splitOn)

trim :: String -> String
trim = let f = reverse . dropWhile isSpace in f . f

dropEmpty :: [String] -> [String]
dropEmpty = filter (not . null) . map trim

commaSeparated :: String -> [String]
commaSeparated = splitOn ","

secondsToMicroseconds :: Double -> Int
secondsToMicroseconds n = round $ n * 1000000

minutesToMicroseconds :: Double -> Int
minutesToMicroseconds n = secondsToMicroseconds $ n * 60
