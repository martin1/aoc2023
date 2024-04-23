module Two(dayResult) where

import Data.List.Split(splitOn)
import Data.Bifunctor (Bifunctor(first))
import Types (DayResult (..))

dayResult :: DayResult
dayResult = DayResult getResult

data Bag = Bag {
    red :: Int,
    green :: Int,
    blue :: Int
} deriving (Eq, Show)

gamePrefix :: String
gamePrefix = "Game "

fullBag :: Bag
fullBag = Bag {red = 12, green = 13, blue = 14}


getResult :: String -> IO (Int, Int)
getResult path = do
    ls <- lines <$> readFile path
    -- sum of ID-s of all possible games
    let res1 = foldr ((\(n,g) acc -> if isGamePossible g then n + acc else acc) . getGameData) 0 ls
    
    let minBags = map (getMinBag . (snd . getGameData)) ls
    -- sum of the power of all minimal bags
    let res2 = foldr (\bag acc -> acc + (red bag * green bag * blue bag)) 0 minBags
    return (res1, res2) 

isGamePossible :: [Bag] -> Bool
isGamePossible = all (\b -> red b <= red fullBag && green b <= green fullBag && blue b <= blue fullBag)

getGameData :: String -> (Int, [Bag])
getGameData s = (gameId, gameData)
    where
        splitResult = splitOn ": " s
        gameId = read $ drop (length gamePrefix) (head splitResult)
        games = splitOn "; " (last splitResult)
        gameData = map (createBag . splitOn ", ") games

getMinBag :: [Bag] -> Bag
getMinBag bags = foldr f (head bags) bags
    where
        f bag acc = acc {
            red = max (red acc) (red bag),
            green = max (green acc) (green bag),
            blue = max (blue acc) (blue bag)
        }

createBag :: [String] -> Bag
createBag = updateBag (Bag 0 0 0)

updateBag :: Bag -> [String] -> Bag
updateBag bag [] = bag
updateBag bag (s:ss)
    | suffix == " red" = updateBag bag {red = n} ss
    | suffix == " green" = updateBag bag {green = n} ss
    | suffix == " blue" = updateBag bag {blue = n} ss
    | otherwise = bag
    where
        (n, suffix) = first (\a -> read a :: Int) (break (== ' ') s)
