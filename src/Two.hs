module Two where
import Data.List.Split
import Data.Bifunctor (Bifunctor(first))

type GameData = [String]

data Bag = Bag {
    red :: Int,
    blue :: Int,
    green :: Int
} deriving (Eq, Show)

gamePrefix :: String
gamePrefix = "Game "


getGameData :: String -> (Int, [GameData])
getGameData s = (gameId, gameData)
    where
        splitResult = splitOn ": " s
        gameId = read $ drop (length gamePrefix) (head splitResult)
        games = splitOn "; " (last splitResult)
        gameData = map (splitOn ", ") games


-- getResult :: String -> Bag -> IO Int
-- getResult s = 

-- 3 blue, etc
updateBag :: String -> Bag -> Bag
updateBag s bag
    | suffix == " red" = bag {red = n}
    | suffix == " green" = bag {green = n}
    | suffix == " blue" = bag {blue = n}
    | otherwise = bag
    where 
        (n, suffix) = first (\a -> read a :: Int) (break (== ' ') s)
