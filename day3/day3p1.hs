data Area = Open | Tree deriving(Eq, Read, Show)
type Map = [[Area]]

main :: IO()
main = do
	input <- readFile "input.txt"
	(print . goThroughMap . createMap . lines) input

goThroughMap :: Map -> Int
goThroughMap = length . (filter isTreeAtPosition) . indexMap
	where
		indexMap = zip [0..]
		isTreeAtPosition (index, line) = (line !! (index*3)) == Tree

createMap :: [String] -> Map
createMap = map repeatingAreas
	where
		repeatingAreas = cycle . (map parseCharToSpace)

parseCharToSpace '.' = Open
parseCharToSpace '#' = Tree
