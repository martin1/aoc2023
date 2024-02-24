module Three(getResult) where

import Data.Char (isDigit)
import Data.List.Split (split)
import Data.List.Split.Internals (whenElt)
import Data.List (partition, intersect)


data NumWithCoords = NumWithCoords
    { num :: String
    , coords :: (Int, Int)
    } deriving (Show)

type SymbolsAt = [(Int, Int)]

getResult :: String -> IO Int
getResult path = do
    ls <- lines <$> readFile path
    let res = sumNums $ filterNums $ concatLineData $ zipWith (curry parseLineData) ls [0..] --map parseLineData $ zip ls [0..]
    return res

getBounds:: NumWithCoords -> [(Int, Int)]
getBounds (NumWithCoords s (x, y)) = [(x', y') |  x' <- [x-1..x + length s], y' <- [y-1..y+1]]

sumNums :: [NumWithCoords] -> Int
sumNums = sum . map ((\n -> read n :: Int) . num)

filterNums :: ([NumWithCoords], SymbolsAt) -> [NumWithCoords]
filterNums (nums, symbolsAt) = filter symbolInBounds nums
    where
        symbolInBounds :: NumWithCoords -> Bool
        symbolInBounds ns = (not . null) $ symbolsAt `intersect` getBounds ns

parseLineData :: (String, Int) -> ([NumWithCoords], SymbolsAt)
parseLineData (s, y') = (numberCoords, symbolsAt)
    where
        list = filter (not . null) (split (whenElt (not . isDigit)) s)
        --indexes need to take into account the length of the strings
        indexes = scanl (\acc e -> acc + length e) 0 list
        listIndexed = filter (\ (a, _) -> a /= ".") $ zip list indexes
        (numbers, symbols) = partition (\(a, _) -> all isDigit a) listIndexed
        numberCoords = map (\(a, x') -> NumWithCoords a (x', y')) numbers
        symbolsAt = map (\(_, x') -> (x', y')) symbols

concatLineData :: [([NumWithCoords], SymbolsAt)] -> ([NumWithCoords], SymbolsAt)
concatLineData = foldr f ([], [])
    where
        f (nums, symbolsAt) (nums', symbolsAt') = (nums' ++ nums, symbolsAt' ++ symbolsAt)
