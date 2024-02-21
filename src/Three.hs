module Three() where

import Data.Char (isDigit)
import Data.List.Split (split)
import Data.List.Split.Internals (whenElt)
import Data.List (partition)


data LineData = LineData {
    nums :: [(String, Int)],
    symbolsAt :: [Int]
} deriving (Eq, Show)


getLineData :: String -> LineData
getLineData s = LineData numbers (map snd symbols)
    where
        list = filter (not . null) (split (whenElt (not . isDigit)) s)
        --indexes need to take into account the length of the strings
        indexes = scanl (\acc x -> acc + length x) 0 list
        listIndexed = filter (\ (a, _) -> a /= ".") $ zip list indexes
        (numbers, symbols) = partition (\(a, _) -> all isDigit a) listIndexed