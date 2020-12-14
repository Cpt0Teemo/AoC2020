import Data.Char
import Data.List
type Entry = (String, [(Int, String)])

main :: IO()
main = do
	dict <- createLookUp . lines <$> readFile "input.txt"
	print . (+ (-1)) . traverseLookUp "shiny gold bag" $ dict

traverseLookUp :: String -> [Entry] -> Int
traverseLookUp bag dict = 1 + numberOfBagsInside
	where
		bagsContained = getBagsContained bag dict
		getAllBagsInBag (quantity, name) = quantity * (traverseLookUp name dict)
		numberOfBagsInside = sum $ map getAllBagsInBag bagsContained

getBagsContained :: String -> [Entry] -> [(Int, String)]
getBagsContained key dict = safeGet (lookup key dict)
	where
		safeGet (Just value) = value
		safeGet Nothing = undefined

createLookUp :: [String] -> [Entry]
createLookUp = map parseEntry

parseEntry :: String -> Entry
parseEntry string = (parseContainer start, parseContained end)
	where 
		(start, end) = break isDigit string

parseContained :: String -> [(Int, String)]
parseContained = map (parseBagWithQuantity . dropPlural . dropWhile (==' ')) . splitAtPunctuation 
	where
		splitAtPunctuation [] = []
		splitAtPunctuation str = let (start, end) = break (`elem` ",.") str in start:(splitAtPunctuation $ tail end)

parseBagWithQuantity :: String -> (Int, String)
parseBagWithQuantity string = (getQuantity string, getName string)
	where
		getQuantity = read . head . words
		getName = unwords . tail . words

parseContainer = dropPlural . unwords . take 3 . words

dropPlural = reverse . dropWhile (=='s') . reverse



