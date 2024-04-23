module Three(dayResult) where

import Data.Char (isDigit)
import Data.List.Split (split)
import Data.List.Split.Internals (whenElt)
import Data.List (partition, intersect)
import Data.Maybe (isJust, fromJust)
import Types (DayResult (..))

dayResult :: DayResult
dayResult = DayResult getResult

data SCoords = SCoords
    { str :: String
    , xy :: (Int, Int)
    } deriving (Show)

type Numbers = [SCoords]
type Symbols = [SCoords]


getResult :: String -> IO (Int, Int)
getResult path = do
    ls <- lines <$> readFile path
    let lineData = concatLineData $ zipWith (curry parseLineData) ls [0..] --map parseLineData $ zip ls [0..]

    let res1 = sumNums $ filterNums lineData 
    let res2 = sumGearRatios $ getPartNumbers lineData

    return (res1, res2)

-- #region part 1

sumNums :: Numbers -> Int
sumNums = sum . map ((\n -> read n :: Int) . str)


filterNums :: (Numbers, Symbols) -> [SCoords]
filterNums (numbers, symbols) = filter symbolInBounds numbers
    where
        symbolInBounds :: SCoords -> Bool
        symbolInBounds ns = (not . null) $ map xy symbols `intersect` getBounds ns

-- #endregion

-- #region part 2

getGearRatios :: (Int, Int) -> Numbers -> Maybe (SCoords, SCoords)
getGearRatios gearCoords numbers
    | length matches == 2 = Just (head matches, last matches)
    | otherwise = Nothing
    where
        isMatch num = gearCoords `elem` getBounds num
        matches = filter isMatch numbers


getPartNumbers :: (Numbers, Symbols) -> [(SCoords, SCoords)]
getPartNumbers (numbers, symbols) = foldr f [] gearCoords
    where
        gears = filter (\ (SCoords s _)-> s == "*") symbols
        gearCoords = map (\ (SCoords _ coords) -> coords) gears
        f gearCoord acc = if isJust partNumbers then fromJust partNumbers : acc else acc
            where
                partNumbers = getGearRatios gearCoord numbers


sumGearRatios:: [(SCoords, SCoords)] -> Int
sumGearRatios nums = sum $ map f nums
    where
        f (SCoords n1 _, SCoords n2 _) = (read n1 :: Int) * (read n2 :: Int)

-- #endregion

-- #region shared

getBounds:: SCoords -> [(Int, Int)]
getBounds (SCoords s (x, y)) = [(x', y') |  x' <- [x-1..x + length s], y' <- [y-1..y+1]]


parseLineData :: (String, Int) -> (Numbers, Symbols)
parseLineData (s, y') = (numberCoords, symbolsAt)
    where
        list = filter (not . null) (split (whenElt (not . isDigit)) s)
        --indexes need to take into account the length of the strings
        indexes = scanl (\acc e -> acc + length e) 0 list
        listIndexed = filter (\ (a, _) -> a /= ".") $ zip list indexes
        (numbers, symbols) = partition (\(a, _) -> all isDigit a) listIndexed
        numberCoords = map (\(a, x') -> SCoords a (x', y')) numbers
        symbolsAt = map (\(a, x') -> SCoords a (x', y')) symbols


concatLineData :: [(Numbers, Symbols)] -> (Numbers, Symbols)
concatLineData = foldr f ([], [])
    where
        f (nums, symbolsAt) (nums', symbolsAt') = (nums' ++ nums, symbolsAt' ++ symbolsAt)

-- #endregion