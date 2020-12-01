main :: IO ()
main = do
	input <- readFile "input.txt"
	(putStrLn . show . getProductOf2020Sum . splitAsIntOnNewLine) input

getProductOf2020Sum list = head [ (snd x) * (snd y) | x <- indexedList, y <- drop (fst x) indexedList, (snd x) + (snd y) == 2020 ]
	where indexedList = zip [1..(length list)] list

splitAsIntOnNewLine = splitAsIntOn '\n'

splitAsIntOn delim string = split string "" []
	where 
		split [] _ acc = acc
		split (x:xs) current acc = if x == delim then (split xs "" ((read (reverse current) :: Int):acc)) else (split xs (x:current) acc)
