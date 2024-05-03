module Seven(dayResult) where
import Types (DayResult (..))
import Data.Containers.ListUtils (nubOrd)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

dayResult :: DayResult
dayResult = DayResult 7 getResult

getResult :: String -> IO (Int, Int)
getResult path = return (0, 0)

type Hand = String
data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind deriving (Eq, Ord, Show)

cards :: [Char]
cards = ['2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A']

validateHand :: String -> Maybe Hand
validateHand s = if length s == 5 && all (`elem` cards) s
    then Just s
    else Nothing

getHandType :: Hand -> HandType
getHandType h = case length charCounts of
    1 -> FiveOfAKind
    2 -> if any ((==4) . snd) charCounts
             then FourOfAKind
             else FullHouse
    3 -> if any ((==3) . snd) charCounts
            then ThreeOfAKind
            else TwoPair
    4 -> OnePair
    5 -> HighCard
    _ -> error "Invalid hand"
    where
        charCounts :: [(Char, Int)]
        charCounts = map (\c -> (c, length $ filter (==c) h)) $ nubOrd h

getStrongerHand :: Hand -> Hand -> Hand
getStrongerHand h1 h2 = case compare h1Type h2Type of
    GT -> h1
    LT -> h2
    EQ -> case compareCards h1 h2 of
        GT -> h1
        LT -> h2
        EQ -> h1
    where
        h1Type = getHandType h1
        h2Type = getHandType h2

compareCards :: Hand -> Hand -> Ordering
compareCards [] _ = EQ
compareCards _ [] = EQ
compareCards (x:xs) (y:ys) = case compare xi yi of
    GT -> GT
    LT -> LT
    EQ -> compareCards xs ys
    where
        xi = fromJust $ elemIndex x cards
        yi = fromJust $ elemIndex y cards


