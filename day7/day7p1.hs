import Data.Char
import Data.List

main :: IO()
main = do
	input <- readFile "input.txt"
	print . (+) (-1) . length . repeatUntilNoChanges ["shiny gold bag"] . lines $ input

repeatUntilNoChanges current strings =
	let result = getAllBagsThatCanHoldOurBag current strings in
	if length current == length result then current else repeatUntilNoChanges result strings
	

getAllBagsThatCanHoldOurBag = foldl' getBagIfHoldsOurBag 

getBagIfHoldsOurBag :: [String] -> String -> [String]
getBagIfHoldsOurBag bags string
	| "no" `elem` (words string) = bags
	| container `elem` bags = bags
	| otherwise = if hasBagsInCommon bags contained then container:bags else bags
	where
		container = parseContainer string
		contained = parseContained string

hasBagsInCommon :: [String] -> [String] -> Bool
hasBagsInCommon [] _ = False
hasBagsInCommon (x:xs) str = if x `elem` str then True else hasBagsInCommon xs str

parseContained string = map (drop 2 . dropPlural . dropWhile (==' ')) . getBags . snd . break isDigit $ string
	where
		getBags [] = []
		getBags str = let (start, end) = break (`elem` ",.") str in start:(getBags $ tail end)

parseContainer = dropPlural . unwords . take 3 . words

dropPlural = reverse . dropWhile (=='s') . reverse



