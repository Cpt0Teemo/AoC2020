main = do
    input <- lines <$> readFile "input.txt"
    let busSchedule = head . tail $ input
    print . findPerfectAlignment . parseIdsWithIndex $ busSchedule

findPerfectAlignment :: [(Int, Int)] -> Int
findPerfectAlignment ids = (product modulos) - (chineseRemainder remainders modulos)
    where
        remainders = map fst ids
        modulos = map snd ids

chineseRemainder :: [Int] -> [Int] -> Int
chineseRemainder remainders modulos = (`mod` modProduct) . sum . zipWith (*) remainders $ multipliers 
    where
        modProduct = product modulos
        factors = map (div modProduct) modulos
        multipliers = zipWith getMultiplier modulos factors


getMultiplier :: Int -> Int -> Int
getMultiplier n m = v * m
    where
        (_:v:_) = besout n m

--Function taken off besout hackage package
besout :: Int-> Int -> [Int]
besout x y = bBesout [1,0,x] [0,1,y]
	where
	bBesout u v = case v!!2 of 0 -> u
		    	           _ -> let q = div (u!!2) (v!!2) in bBesout v [u!!k - q * v!!k | k <- [0..2]]

parseIdsWithIndex :: String -> [(Int, Int)]
parseIdsWithIndex = map castSnd . filter isNotXOnSnd . zip [0..] . reverse . parseCsv
    where 
        isNotXOnSnd (_, value) = value /= "x"
        castSnd (i, x) = (i, read x)

parseCsv :: String -> [String]
parseCsv = parseCsv' []
    where
        parseCsv' acc [] = acc
        parseCsv' acc csv = parseCsv' ((first csv):acc) (rest csv)
        first = takeWhile (/=',')
        rest = safeTail . dropWhile (/=',')

safeTail [] = []
safeTail list = tail list