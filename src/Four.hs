module Four(getResult) where
import Data.List (intersect)
import Data.Tree (Tree, unfoldTree, flatten)
import Data.Maybe (fromMaybe)

type WinningNumbers = [Int]
type LocalNumbers = [Int]

type LineNo = Int
type MatchCount = Int

getResult :: String -> IO (Int, Int)
getResult path = do
    ls <- lines <$> readFile path
    let res1 = foldr (\l acc -> acc + getLinePoints l) 0 ls
    let res2 = totalCardCount ls
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
            else (i, [i+1..i + getMatchCount i])
        getMatchCount i = fromMaybe 0 (lookup i matches)


totalCardCount :: [String] -> Int
totalCardCount ls = sum treeCardCounts
    where
        matchCounts = getMatchCounts ls
        lineNos = zipWith const [1..] ls
        trees = map (`buildTree` matchCounts) lineNos
        treeCardCounts = map (length . flatten) trees
