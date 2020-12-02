data Password = Password { policy :: (Int, Int), letter :: Char, password :: String } deriving(Eq, Show, Read)

main :: IO()
main = do
	input <- readFile "input.txt"
	(putStrLn . show . countValidPasswordsFromStrings . lines) input

countValidPasswordsFromStrings :: [String] -> Int
countValidPasswordsFromStrings = length . filter (validatePassword . parsePassword)

validatePassword :: Password -> Bool
validatePassword pwd = lowest <= reps && reps <= highest
	where 
		reps = countRepetition (letter pwd) (password pwd)
		lowest = (fst . policy) pwd
		highest = (snd . policy) pwd

parsePassword :: String -> Password
parsePassword string = Password { policy=(lowest, highest), letter=char, password=pwd}
	where
		lowest = (read . (takeWhile (/= '-'))) string
		highest = (read . (takeWhile (/= ' ')) . tail . (dropWhile (/= '-'))) string
		char = (last . (takeWhile (/= ':'))) string
		pwd = (tail . tail . (dropWhile (/= ':'))) string

countRepetition :: Char -> String -> Int
countRepetition char string = foldr (\x acc -> if x == char then acc+1 else acc) 0 string 
