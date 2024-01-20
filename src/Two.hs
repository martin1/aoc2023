module Two where
import Data.List.Split

data Bag = Bag {
    red :: Int,
    blue :: Int,
    green :: Int
} deriving (Eq, Show)

type GameData = [String]

gamePrefix :: String
gamePrefix = "Game "


getGameData :: String -> (Int, [GameData])
getGameData s = (gameId, games)
    where
        splitResult = splitOn ": " s
        gameId = read $ drop (length gamePrefix) (head splitResult)
        games = splitOn "; " (last splitResult)

-- getResult :: String -> Bag -> IO Int
-- getResult s = 

-- 3 blue, etc
updateBag :: (Int, String) -> Bag -> Bag
updateBag (n, suffix) bag
    | suffix == " red" = bag {red = n}
    | suffix == " green" = bag {green = n}
    | suffix == " blue" = bag {blue = n}
    | otherwise = bag
