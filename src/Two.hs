module Two where
import Types(Bag(..))
import Data.Char (isDigit)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Text (splitOn)

-- getResult :: String -> Bag -> IO Int
-- getResult s = 

-- parseBag :: [String] -> Bag
-- parseBag [] = Bag 0 0 0
-- parseBag strs = foldr f (Bag 0 0 0) strs
--     where
--         f s acc = suffix
--             | " red"
--             where
--                 let (n, suffix) = break (== ' ') s
--             red = " red"
--             green = " green"
--             blue = " blue"
-- 3 blue, etc
updateBag :: (Int, String) -> Bag -> Bag
updateBag (n, suffix) bag
    | suffix == " red" = bag {red = n}
    | suffix == " green" = bag {green = n}
    | suffix == " blue" = bag {blue = n}
    | otherwise = bag
