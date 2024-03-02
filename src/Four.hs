module Four(getResult) where
import Data.List (intersect)
import Data.Tree (Tree, unfoldTree)
import Data.Maybe (fromMaybe)

type WinningNumbers = [Int]
type LocalNumbers = [Int]

type LineNo = Int
type MatchCount = Int

getResult :: String -> IO (Int, Int)
getResult path = do
    ls <- lines <$> readFile path
    let res1 = foldr (\l acc -> acc + getLinePoints l) 0 ls
    let res2 = 0
    return (res1, res2)


getLinePoints :: String -> Int
getLinePoints = points . getWinningNumbers . getLineNumbers
    where
        points [] = 0
        points ns = 2 ^ (length ns - 1)



getWinningNumbers :: (WinningNumbers, LocalNumbers) -> [Int]
getWinningNumbers (ws, ls) =  ws `intersect` ls

getLineNumbers :: String -> (WinningNumbers, LocalNumbers)
getLineNumbers s = (toNumbers leftNums, toNumbers rightNums)
    where
        (_, numString) = breakOn ':' s
        (leftNums, rightNums) = breakOn '|' numString
        toNumbers = map (\x -> read x :: Int) . words

breakOn :: Char -> String -> (String, String)
breakOn c s = case break (== c) s of
    (l, _:r) -> (l, r)
    (l, [])  -> (l, [])

getMatchCounts :: [String] -> [(LineNo, MatchCount)]
getMatchCounts = zipWith (curry getLineMatchCount) [1..]

getLineMatchCount :: (Int, String) -> (LineNo, MatchCount)
getLineMatchCount (index, line) = (index, matchCount line)
    where
        matchCount = length . getWinningNumbers . getLineNumbers

buildTree :: LineNo -> [(LineNo, MatchCount)] -> Tree Int
buildTree lineNo matches = unfoldTree buildNode lineNo
    where
        buildNode i = if getMatchCount i == 0 
            then (i, []) 
            else (i, [i+1.. i + getMatchCount i])
        getMatchCount i = fromMaybe 0 (lookup i matches)

testLines :: [String]
testLines = [
    "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53",
    "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19",
    "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1",
    "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83",
    "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36",
    "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
    ]