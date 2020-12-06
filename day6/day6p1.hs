import Data.List

main :: IO()
main = do
	input <- readFile "input.txt"
	(print . sum . map (length . nub) . getGroupsOfStrings . lines) input

getGroupsOfStrings :: [String] -> [String]
getGroupsOfStrings lines = getGroupsOfStrings' lines [] []
getGroupsOfStrings' [] curr acc = curr:acc
getGroupsOfStrings' ("":xs) curr acc = getGroupsOfStrings' xs [] (curr:acc)
getGroupsOfStrings' (x:xs) curr acc = getGroupsOfStrings' xs (x++curr) acc
