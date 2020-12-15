main :: IO()
main = do
    input <- map (read :: String -> Int) . lines <$> readFile "input.txt"
    print . getMinMaxSum . findContiguousSet input [] $ run (take 25 input) (drop 25 input)

run :: [Int] -> [Int] -> Int
run history (x:xs) = if x `isSumOfTwo` history then run (updateHistory history x) xs else x

updateHistory :: [Int] -> Int -> [Int]
updateHistory history value = drop 1 (history ++ [value])

isSumOfTwo :: Int -> [Int] -> Bool
isSumOfTwo value list = not . null $ [(x,y) | (i, x) <- (zip [1..] list), y <- (drop i list), x + y == value]

findContiguousSet :: [Int] -> [Int] -> Int -> [Int]
findContiguousSet (x:xs) currSet value
    | x >= value             = findContiguousSet xs [] value
    | currValue + x == value = x:currSet
    | currValue + x < value  = findContiguousSet xs (currSet++[x]) value
    | otherwise              =  findContiguousSet (x:xs) (tail currSet) value
    where
        currValue = sum currSet

getMinMaxSum :: [Int] -> Int
getMinMaxSum list = sum $ map ($ list) [minimum, maximum]