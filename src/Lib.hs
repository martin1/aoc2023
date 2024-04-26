module Lib(getNumbers) where
import Data.Char (isDigit)

getNumbers :: String -> [Int]
getNumbers s = map read $ filter (all isDigit) $ words s