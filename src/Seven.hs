module Seven(dayResult) where
import Types (DayResult (..))
import Data.Char (isAlphaNum)
import Data.Containers.ListUtils (nubOrd)

dayResult :: DayResult
dayResult = DayResult 7 getResult

getResult :: String -> IO (Int, Int)
getResult path = return (0, 0)

type Hand = String
data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind deriving (Eq, Ord, Show)

validateHand :: String -> Maybe Hand
validateHand s = if length s == 5 && all isAlphaNum s
    then
        Just s
    else 
        Nothing

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

