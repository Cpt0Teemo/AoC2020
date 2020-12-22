import Data.List
import Data.Maybe (catMaybes)
import qualified Data.Map as Map

type Index = (Int, Int)

main :: IO()
main = do 
    input <- readFile "input.txt"
    let width = length . takeWhile (/= '\n') $ input
    let height = length . lines $ input
    let list = filter (/= '\n') input
    let dict = Map.fromList (indexList width list)
    print . length . filter ((=='#') . snd) . Map.toList . runTillStable (invertMap updateChar) $ dict

indexList :: Int -> [Char] -> [(Index, Char)]
indexList width = zip indexes
    where indexes = [(x `mod` width, x `div` width) | x <- [0..]]

invertMap :: (Map.Map Index Char -> Index -> Char -> Char) -> Map.Map Index Char -> Map.Map Index Char
invertMap update dict = Map.mapWithKey (update dict) dict

updateChar :: Map.Map Index Char -> Index -> Char -> Char
updateChar _ _ '.' = '.'
updateChar dict index 'L' = if countAdjacentOccupied dict index == 0 then '#' else 'L'
updateChar dict index '#' = if countAdjacentOccupied dict index > 4 then 'L' else '#'

countAdjacentOccupied :: Map.Map Index Char -> Index -> Int
countAdjacentOccupied dict = length . filter (=='#') . getSeatsInSight dict 

getSeatsInSight :: Map.Map Index Char -> Index -> [Char]
getSeatsInSight dict index@(col, row) = catMaybes . map (getTillSeat dict index) $ angles
    where angles = [(x,y) | x <- [-1..1], y <- [-1..1], (x,y)/=(0,0)]

getTillSeat :: Map.Map Index Char -> Index -> (Int ,Int) -> Maybe Char
getTillSeat dict (col, row) (colInc, rowInc) =
    case Map.lookup newIndex dict of
        Nothing -> Nothing
        Just '.' -> getTillSeat dict newIndex (colInc, rowInc)
        value -> value
    where newIndex = (col+colInc, row+rowInc)

runTillStable f a = if a == a' then a' else runTillStable f a'
    where a' = f a