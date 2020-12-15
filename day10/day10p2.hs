import Data.List

main :: IO()
main = do
    input <- (0 :) . map (read :: String -> Int) . lines <$> readFile "input.txt"
    print .  run [] . sort $ input

run :: [(Int, Int)] -> [Int] -> Int
run acc [] = snd (head acc)
run [] (x:xs) = run [(x, 1)] xs
run acc (x:xs) = run ((x, getTotalQuantity feasibleOptions) : feasibleOptions) xs
    where
        feasibleOptions = filter (\ option -> (x - 3) <= (fst option)) acc
        getTotalQuantity = sum . map snd
