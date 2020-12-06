import Data.List

main :: IO()
main = do
	input <- readFile "input.txt"
	(print . sum . map (length) . getAnsweredByWholeGroup . getGroupsOfStrings . lines) input

getAnsweredByWholeGroup = map (foldl (\ x y -> intersect x y) "abcdefghijklmnopqrstuvwxyz")

getGroupsOfStrings :: [String] -> [[String]]
getGroupsOfStrings lines = getGroupsOfStrings' lines [] []
getGroupsOfStrings' [] curr acc = curr:acc
getGroupsOfStrings' ("":xs) curr acc = getGroupsOfStrings' xs [] (curr:acc)
getGroupsOfStrings' (x:xs) curr acc = getGroupsOfStrings' xs (x:curr) acc
