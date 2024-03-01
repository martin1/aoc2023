module Four(getResult) where
import Data.List (intersect)

type WinningNumbers = [Int]
type LocalNumbers = [Int]

getResult :: String -> IO (Int, Int)
getResult path = do
    ls <- lines <$> readFile path
    let res1 = foldr (\l acc -> acc + getLinePoints l) 0 ls
    let res2 = 0
    return (res1, res2)


getLinePoints :: String -> Int
getLinePoints line = points $ getWinningNumbers $ getLineNumbers line
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
