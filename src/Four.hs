module Four where


getLineNumbers :: String -> ([Int], [Int])
getLineNumbers s = (toNumbers leftNums, toNumbers rightNums)
    where
        (_, numString) = breakOn ':' s
        (leftNums, rightNums) = breakOn '|' numString
        toNumbers = map (\x -> read x :: Int) . words

breakOn :: Char -> String -> (String, String)
breakOn c s = case break (== c) s of
    (l, _:r) -> (l, r)
    (l, [])  -> (l, [])


--Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53 