module Three(getResult) where

import Data.Char (isDigit)
import Data.List.Split (split)
import Data.List.Split.Internals (whenElt)
import Data.List (partition, intersect)

data SCoords = SCoords
    { str :: String
    , xy :: (Int, Int)
    } deriving (Show)

type Numbers = [SCoords]
type Symbols = [SCoords]


getResult :: String -> (IO Int,IO Int)
getResult path = (res1, res2)
    where
        res1 = do
            ls <- lines <$> readFile path
            let res = sumNums $ filterNums $ concatLineData $ zipWith (curry parseLineData) ls [0..] --map parseLineData $ zip ls [0..]
            return res
        res2 = do
            return 0 -- TODO
    

getBounds:: SCoords -> [(Int, Int)]
getBounds (SCoords s (x, y)) = [(x', y') |  x' <- [x-1..x + length s], y' <- [y-1..y+1]]

sumNums :: Numbers -> Int
sumNums = sum . map ((\n -> read n :: Int) . str)

filterNums :: (Numbers, Symbols) -> [SCoords]
filterNums (numbers, symbols) = filter symbolInBounds numbers
    where
        symbolInBounds :: SCoords -> Bool
        symbolInBounds ns = (not . null) $ map xy symbols `intersect` getBounds ns

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
