import Data.List

main :: IO()
main = do
    input <- (0 :) . map (read :: String -> Int) . lines <$> readFile "input.txt"
    print . run . sort $ input

run :: [Int] -> Int
run = pairProduct . foldl' storeDiff (0,0) . sequentialPairs
    where 
        storeDiff (smallDiff, largeDiff) (prev, curr) = 
            case curr - prev of
                1 -> (smallDiff + 1, largeDiff)
                3 -> (smallDiff, largeDiff + 1)
                _ -> (smallDiff, largeDiff)
        sequentialPairs list = zip list (tail list)
        pairProduct (x,y) = x * (y+1)
