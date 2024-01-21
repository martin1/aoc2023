module Two(getResult) where

import Data.List.Split(splitOn)
import Data.Bifunctor (Bifunctor(first))

data Bag = Bag {
    red :: Int,
    green :: Int,
    blue :: Int
} deriving (Eq, Show)

gamePrefix :: String
gamePrefix = "Game "

fullBag :: Bag
fullBag = Bag {red = 12, green = 13, blue = 14}


getResult :: String -> IO Int
getResult path = fmap (foldr ((\(n,g) acc -> if isGamePossible g then n + acc else acc) . getGameData) 0) (lines <$> readFile path)
    -- do
    -- ls <- lines <$> readFile path
    -- let b = foldr ((\(n,g) acc -> if isGamePossible g then n + acc else acc) . getGameData) 0 ls
    -- return b


isGamePossible :: [Bag] -> Bool
isGamePossible = all (\b -> red b <= red fullBag && green b <= green fullBag && blue b <= blue fullBag)

getGameData :: String -> (Int, [Bag])
getGameData s = (gameId, gameData)
    where
        splitResult = splitOn ": " s
        gameId = read $ drop (length gamePrefix) (head splitResult)
        games = splitOn "; " (last splitResult)
        gameData = map (createBag . splitOn ", ") games

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
