type Range = (Int, Int)

main :: IO()
main = do
	input <- readFile "input.txt"
	(print . getHighestSeatId . lines) input

getHighestSeatId :: [String] -> Int
getHighestSeatId = (foldl max 0) . (map getSeatId)

getSeatId :: [Char] -> Int
getSeatId list = col * 8 + row
	where 
		col = binarySearchColumn list
		row = binarySearchRow list

binarySearchColumn :: [Char] -> Int
binarySearchColumn = fst . (foldl binSearch (0, 127)) . getColumnInputs

binarySearchRow :: [Char] -> Int
binarySearchRow = fst . (foldl binSearch (0, 7)) . getRowInputs

getColumnInputs :: [Char] -> [Char]
getColumnInputs = filter (`elem` ['F','B'])

getRowInputs :: [Char] -> [Char]
getRowInputs = filter (`elem` ['L','R'])

binSearch :: Range -> Char -> Range
binSearch (low, high) char
	| char == 'F' || char == 'L' = (low, center)
	| char == 'B' || char == 'R' = (center + 1, high)
	where center = low + ((high - low) `div` 2)
