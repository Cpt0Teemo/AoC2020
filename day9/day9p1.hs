main :: IO()
main = do
    input <- map (read :: String -> Int) . lines <$> readFile "input.txt"
    print $ run (take 25 input) (drop 25 input)

run :: [Int] -> [Int] -> Int
run history (x:xs) = if x `isSumOfTwo` history then run (updateHistory history x) xs else x

updateHistory :: [Int] -> Int -> [Int]
updateHistory history value = drop 1 (history ++ [value])

isSumOfTwo :: Int -> [Int] -> Bool
isSumOfTwo value list = not . null $ [(x,y) | (i, x) <- (zip [1..] list), y <- (drop i list), x + y == value]