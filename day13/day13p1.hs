main = do
    input <- lines <$> readFile "input.txt"
    let earliestTime = read . head $ input
    let busSchedule = head . tail $ input
    print . (\ (id, time) -> id * (time - earliestTime)) . getEarliestBus earliestTime . parseSchedule $ busSchedule

getEarliestBus :: Int -> [Int] -> (Int, Int)
getEarliestBus time = foldl1 min' . map (\x -> (x, getNextTimeAfter time x))
    where min' acc@(_,x) element@(_,y) = if x > y then element else acc

getNextTimeAfter :: Int -> Int -> Int
getNextTimeAfter time id = multiple * id
    where multiple = (time `div` id) + 1

parseSchedule :: String -> [Int]
parseSchedule = map (read :: String -> Int) . filter (/="x") . parseCsv

parseCsv :: String -> [String]
parseCsv = parseCsv' []
    where
        parseCsv' acc [] = acc
        parseCsv' acc csv = parseCsv' ((first csv):acc) (rest csv)
        first = takeWhile (/=',')
        rest = safeTail . dropWhile (/=',')

safeTail [] = []
safeTail list = tail list