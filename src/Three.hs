module Three() where

import Data.Char (isDigit)
import Data.List.Split (split)
import Data.List.Split.Internals (whenElt)
import Data.List (partition)


getResult :: [String] -> ([(String, (Int, Int))], [(Int, Int)])
getResult ls = foldr f ([], []) (zip ls [0..])
    where
        f (s, i) (nums, symbolsAt) = (nums ++ lineNums, symbolsAt ++ lineSymbolsAt)
            where
                (lineNums, lineSymbolsAt) = getLineData s i



getLineData :: String -> Int -> ([(String, (Int, Int))], [(Int, Int)])
getLineData s i = (numberCoords, symbolCoords)
    where
        list = filter (not . null) (split (whenElt (not . isDigit)) s)
        --indexes need to take into account the length of the strings
        indexes = scanl (\acc x -> acc + length x) 0 list
        listIndexed = filter (\ (a, _) -> a /= ".") $ zip list indexes
        (numbers, symbols) = partition (\(a, _) -> all isDigit a) listIndexed
        numberCoords = map (\(a, x) -> (a, (x, i))) numbers
        symbolCoords = map (\(_, x) -> (x, i)) symbols


testlines :: [String]
testlines =
    [ "467..114.."
    , "...*......"
    , "..35..633."
    , "......#..."
    , "617*......"
    , ".....+.58."
    , "..592....."
    , "......755."
    , "...$.*...."
    , ".664.598.."
    ]
