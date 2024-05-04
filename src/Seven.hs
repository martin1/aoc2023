module Seven(dayResult) where
import Types (DayResult (..))
import Data.Containers.ListUtils (nubOrd)
import Data.List ( elemIndex, sortOn )
import Data.Maybe (fromJust)

dayResult :: DayResult
dayResult = DayResult 7 getResult

getResult :: String -> IO (Int, Int)
getResult path = do
    ls <- lines <$> readFile path
    let handBids = map getHandAndBid ls
    let sortedBids = map snd $ sortOn fst handBids
    let res1 = foldr (\(a, b) acc -> acc + (a * b)) 0 (zip sortedBids [1..])
    return (res1, 0)

newtype Hand = Hand String deriving (Eq, Show)

instance Ord Hand where
    compare h1 h2 = case compare h1Type h2Type of
        GT -> GT
        LT -> LT
        EQ -> case compareCards h1 h2 of
            GT -> GT
            LT -> LT
            EQ -> GT
        where
            h1Type = getHandType h1
            h2Type = getHandType h2

data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind deriving (Eq, Ord, Show)

cards :: [Char]
cards = ['2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A']

validateHand :: String -> Maybe Hand
validateHand s = if length s == 5 && all (`elem` cards) s
    then Just $ Hand s
    else Nothing

getHandType :: Hand -> HandType
getHandType (Hand h) = case length charCounts of
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

compareCards :: Hand -> Hand -> Ordering
compareCards (Hand []) _ = EQ
compareCards _ (Hand []) = EQ
compareCards (Hand (x:xs)) (Hand (y:ys)) = case compare xi yi of
    GT -> GT
    LT -> LT
    EQ -> compareCards (Hand xs) (Hand ys)
    where
        xi = fromJust $ elemIndex x cards
        yi = fromJust $ elemIndex y cards

getHandAndBid :: String -> (Hand, Int)
getHandAndBid s = (h, b)
    where
        ws = words s
        h = fromJust $ validateHand $ head ws
        b = read $ last ws
