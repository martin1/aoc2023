module Six(dayResult) where
import Types (DayResult (..))
import Lib (getNumbers)

dayResult :: DayResult
dayResult = DayResult 6 getResult

getResult :: String -> IO (Int, Int)
getResult path = do
    ls <- lines <$> readFile path
    let races = getRaces (head ls, ls !! 1)
    let res1 = foldr ((*) . getNumRecords) 1 races

    let race = getRace (head ls, ls !! 1)
    let res2 = getNumRecords race
    return (res1, res2)

data Race = Race {
    time:: Int,
    distance :: Int
} deriving (Eq, Show)

getRaces :: (String, String) -> [Race]
getRaces (times, dists) = [Race t d | (t, d) <- zip ts ds]
    where
        ts = getNumbers times
        ds = getNumbers dists

getRace :: (String, String) -> Race
getRace (times, dists) = Race (toNum ts) (toNum ds)
    where
        ts = getNumbers times
        ds = getNumbers dists
        toNum ns = read (concatMap show ns) :: Int

getNumRecords :: Race -> Int
getNumRecords (Race t d) = length records
    where
        possibleRaces = [ (t - n) * n | n <- [1..t - 1] ]
        records = filter (> d) possibleRaces
