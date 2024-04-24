module Six(dayResult) where
import Types (DayResult (..))
import Lib (getNumbers)

dayResult :: DayResult
dayResult = DayResult 6 (\_ -> return (0, 0))

data Race = Race {
    time:: Int,
    distance :: Int
} deriving (Eq, Show)

getRaces :: (String, String) -> [Race]
getRaces (times, dists) = [Race t d | (t, d) <- zip ts ds]
    where
        ts = getNumbers times
        ds = getNumbers dists

ls :: [String]
ls = [
    "Time:      7  15   30",
    "Distance:  9  40  200"
    ]