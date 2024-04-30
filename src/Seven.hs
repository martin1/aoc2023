module Seven(dayResult) where
import Types (DayResult (..))
import Data.Char (isAlphaNum)
import Data.Containers.ListUtils (nubOrd)

dayResult :: DayResult
dayResult = DayResult 7 getResult

getResult :: String -> IO (Int, Int)
getResult path = return (0, 0)

type Hand = String
data HandType = FiveOfAKind | FourOfAKind | FullHouse | ThreeOfAKind | TwoPair | OnePair | HighCard

validateHand :: String -> Maybe Hand
validateHand s = if length s == 5 && all isAlphaNum s
    then
        Just s
    else 
        Nothing

getHandType :: Hand -> HandType
getHandType h = case uniqueCount of
    1 -> FiveOfAKind
    2 -> FourOfAKind
    -- todo
    _ -> HighCard
    where 
        uniqueCount = length $ nubOrd h

