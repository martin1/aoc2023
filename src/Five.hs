module Five (dayResult) where
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.List.Split (splitOn)
import Types (DayResult(..))
import Lib (getNumbers)

dayResult :: DayResult
dayResult = DayResult 5 getResult


data MapRange = MapRange {
    destStart :: Int,
    sourceStart :: Int,
    len :: Int
} deriving Show

data SeedList = Seeds {
    start :: Int,
    count :: Int
} deriving Show

getResult :: String -> IO (Int, Int)
getResult path = do
    ls <- lines <$> readFile path

    let seedStr = head ls
    let seeds = getSeeds seedStr

    let maps = map snd . getMaps $ drop 2 ls

    let valuesForSeeds =  map (`valueForSeed` maps) seeds
    let res1 = minimum valuesForSeeds

    let seedLists = getSeeds' seedStr
    let valuesForSeedLists = map (`valueForSeedList` maps) seedLists

    let res2 = minimum valuesForSeedLists

    return (res1, res2)

getSeeds:: String -> [Int]
getSeeds s = if "seeds: " `isPrefixOf` s
    then
        getNumbers s
    else
        error "Invalid input: seeds not found"

getSeeds':: String -> [SeedList]
getSeeds' s = if odd (length seeds)
    then
        error "Invalid input"
    else
        makeSeedList . getSeeds $ s
        where
            seeds = getSeeds s

makeSeedList :: [Int] -> [SeedList]
makeSeedList [] = []
makeSeedList [a] = [Seeds {start = a, count = 1}]
makeSeedList [a,b] = [Seeds {start = a, count = b}]
makeSeedList (a:b:rest) = Seeds {start = a, count = b} : makeSeedList rest

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

valueForSeedList:: SeedList -> [[MapRange]] -> Int
valueForSeedList sl maps = minimum values
    where
        seeds = [start sl..start sl + count sl - 1]
        values = map (`valueForSeed` maps) seeds