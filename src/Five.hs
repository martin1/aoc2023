module Five (getResult) where
import Data.Char (isDigit)
import Data.List (isPrefixOf)


data MapRange = MapRange {
    destStart :: Int,
    sourceStart :: Int,
    len :: Int
} deriving Show

getResult :: String -> IO (Int, Int)
getResult _ = return (0, 0) -- TODO

getSeeds:: String -> [Int]
getSeeds s = if "seeds: " `isPrefixOf` s
    then getNumbers s
    else error "Invalid input: seeds not found"


--50 98 2
--52 50 48

getNumbers :: String -> [Int]
getNumbers s = map read $ filter (all isDigit) $ words s

getMapRange :: String -> MapRange
getMapRange s = if length nums /= 3
    then error "Invalid input: expecting exactly 3 numbers"
    else
        MapRange {
            destStart = head nums,
            sourceStart = nums !! 1,
            len = nums !! 2
        }
    where
        nums = getNumbers s
