import Data.List

data Password = Password { policy :: (Int, Int), letter :: Char, password :: String } deriving(Eq, Show, Read)

main :: IO()
main = do
	input <- readFile "input.txt"
	(putStrLn . show . countValidPasswordsFromStrings . lines) input

countValidPasswordsFromStrings :: [String] -> Int
countValidPasswordsFromStrings = length . filter (validatePassword . parsePassword)

validatePassword :: Password -> Bool
validatePassword pwd = isAtFirstPosition `xor` isAtSecondPosition
	where 
		isAtFirstPosition = firstIndex `elem` (elemIndices char (password pwd))
		isAtSecondPosition = secondIndex `elem` (elemIndices char (password pwd))
		char = letter pwd
		firstIndex = ((fst . policy) pwd) - 1
		secondIndex = ((snd . policy) pwd) - 1

parsePassword :: String -> Password
parsePassword string = Password { policy=(lowest, highest), letter=char, password=pwd}
	where
		lowest = (read . (takeWhile (/= '-'))) string
		highest = (read . (takeWhile (/= ' ')) . tail . (dropWhile (/= '-'))) string
		char = (last . (takeWhile (/= ':'))) string
		pwd = (tail . tail . (dropWhile (/= ':'))) string

xor val1 val2 = (val1 && not val2) || (val2 && not val1)
