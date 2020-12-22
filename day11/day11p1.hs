import Data.List
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
updateChar dict index 'L' = if isIsolated dict index then '#' else 'L'
updateChar dict index '#' = if countAdjacentOccupied dict index > 3 then 'L' else '#'

countAdjacentOccupied :: Map.Map Index Char -> Index -> Int
countAdjacentOccupied dict = length . filter (not . isFreeInDict dict) . getIndexesAdjacent

isIsolated :: Map.Map Index Char -> Index -> Bool
isIsolated dict = all (isFreeInDict dict) . getIndexesAdjacent

isFreeInDict :: Map.Map Index Char -> Index -> Bool
isFreeInDict dict = isFree . (`Map.lookup` dict)

getIndexesAdjacent :: Index -> [Index]
getIndexesAdjacent (col, row) = [(x, y) | x <- [(col-1)..(col+1)], y <- [(row-1)..(row+1)], (x,y) /= (col, row)]

isFree :: Maybe Char -> Bool
isFree (Just '#') = False
isFree _ = True

runTillStable f a = if a == a' then a' else runTillStable f a'
    where a' = f a