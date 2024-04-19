module Five (getResult) where
import Data.Char (isDigit)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.List.Split (splitOn)


data MapRange = MapRange {
    destStart :: Int,
    sourceStart :: Int,
    len :: Int
} deriving Show

getResult :: String -> IO (Int, Int)
getResult path = do
    ls <- lines <$> readFile path

    let seeds = getSeeds $ head ls
    let maps = map snd . getMaps $ drop 2 ls
    let valuesForSeeds =  map (`valueForSeed` maps) seeds
    let res1 = minimum valuesForSeeds

    return (res1, 0)

getSeeds:: String -> [Int]
getSeeds s = if "seeds: " `isPrefixOf` s
    then getNumbers s
    else error "Invalid input: seeds not found"


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

getMapValue :: [MapRange] -> Int -> Int
getMapValue [] n = n
getMapValue (r:rs) n = fromMaybe (getMapValue rs n) (getRangeValue r n)

getRangeValue :: MapRange -> Int -> Maybe Int
getRangeValue range n = case dstOffset of
    Just offset -> Just (n + offset)
    Nothing -> Nothing
    where
        srcStart = sourceStart range
        srcEnd = srcStart + len range - 1
        dstStart = destStart range

        dstOffset = if n < srcStart || n > srcEnd
            then 
                Nothing
            else 
                Just (dstStart - srcStart)

getMaps :: [String] -> [(String, [MapRange])]
getMaps ls = map f mapData
    where
        mapData :: [[String]]
        mapData = splitOn [""] ls

        f :: [String] -> (String, [MapRange])
        f [] = ("", [])
        f (x:xs) = (x, map getMapRange xs)


valueForSeed:: Int -> [[MapRange]] -> Int
valueForSeed = foldl (flip getMapValue)
-- valueForSeed seed [] = seed
-- valueForSeed seed (map:maps) = valueForSeed (getMapValue map seed) (maps)


testlines :: [String]
testlines = [
    "seeds: 79 14 55 13",
    "",
    "seed-to-soil map:",
    "50 98 2",
    "52 50 48",
    "",
    "soil-to-fertilizer map:",
    "0 15 37",
    "37 52 2",
    "39 0 15",
    "",
    "fertilizer-to-water map:",
    "49 53 8",
    "0 11 42",
    "42 0 7",
    "57 7 4",
    "",
    "water-to-light map:",
    "88 18 7",
    "18 25 70",
    "",
    "light-to-temperature map:",
    "45 77 23",
    "81 45 19",
    "68 64 13",
    "",
    "temperature-to-humidity map:",
    "0 69 1",
    "1 0 69",
    "",
    "humidity-to-location map:",
    "60 56 37",
    "56 93 4",
    ""
    ]
