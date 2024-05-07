module Seven(dayResult) where
import Types (DayResult (..))
import Data.List ( elemIndex, sortOn )
import Data.Maybe (fromJust)
import Data.Ord ( Down(Down) )
import Data.Containers.ListUtils (nubOrd)

dayResult :: DayResult
dayResult = DayResult 7 getResults

getResults :: String -> IO (Int, Int)
getResults path = do
    ls <- lines <$> readFile path

    let res1 = getResult ls False
    let res2 = getResult ls True
    return (res1, res2)

getResult :: [String] -> Bool -> Int
getResult ls applyJ = foldr (\(a, b) acc -> acc + (a * b)) 0 (zip sortedBids [1..])
    where
        handBids = map (`getHandAndBid` applyJ) ls
        sortedBids = map snd $ sortOn fst handBids
        

data Hand = Hand String Bool deriving (Eq, Show)

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

validateHand :: String -> Bool -> Maybe Hand
validateHand s applyJ = if length s == 5 && all (`elem` cards) s
    then Just $ Hand s applyJ
    else Nothing

getHandType :: Hand -> HandType
getHandType (Hand h applyJ) = case length $ freqList hand of
    1 -> FiveOfAKind
    2 -> if any ((==4) . snd) $ freqList hand
             then FourOfAKind
             else FullHouse
    3 -> if any ((==3) . snd) $ freqList hand
            then ThreeOfAKind
            else TwoPair
    4 -> OnePair
    5 -> HighCard
    _ -> error "Invalid hand"
    where
        hand = if applyJ && not (all (=='J') h)
            then
                let char = mostFrequentChar $ filter (/= 'J') h
                    replaceJ c = if c == 'J'
                        then char
                        else c
                in map replaceJ h
            else h

mostFrequentChar :: String -> Char
mostFrequentChar s = head (maxByFreq s)
  where
    maxByFreq :: String -> [Char]
    maxByFreq = map fst . sortOn (Down . snd) . freqList


freqList :: String -> [(Char, Int)]
freqList s = map (\c -> (c, length $ filter (==c) s)) $ nubOrd s


compareCards :: Hand -> Hand -> Ordering
compareCards (Hand [] _) _ = EQ
compareCards _ (Hand [] _) = EQ
compareCards (Hand (x:xs) applyJ) (Hand (y:ys) _) = case compare xi yi of
    GT -> if applyJ && x == 'J'
        then LT
        else GT
    LT -> if applyJ && y == 'J'
        then GT
        else LT
    EQ -> compareCards (Hand xs applyJ) (Hand ys applyJ)
    where
        xi = fromJust $ elemIndex x cards
        yi = fromJust $ elemIndex y cards

getHandAndBid :: String -> Bool -> (Hand, Int)
getHandAndBid s applyJ = (h, b)
    where
        ws = words s
        h = fromJust $ validateHand (head ws) applyJ
        b = read $ last ws
