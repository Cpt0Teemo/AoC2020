data Area = Open | Tree deriving(Eq, Read, Show)
type Map = [[Area]]
type Slope = (Int, Int)

main :: IO()
main = do
	input <- readFile "input.txt"
	let slopes = [(1,1),(1,3),(1,5),(1,7),(2,1)]
	let nMaps = makeNMaps (length slopes) (createMap (lines input))
	let nFunctions = map goThroughMap slopes
	let pairedUp = zip nFunctions nMaps
	print $ product (map (\ (f, map) -> f map) pairedUp)

makeNMaps :: Int -> Map -> [Map]
makeNMaps = replicate

goThroughMap :: Slope -> Map -> Int
goThroughMap (down, right) = length . (filter isTreeAtPosition) . indexMap . (removeAtModN down)
	where
		indexMap = zip [0..]
		isTreeAtPosition (index, line) = (line !! (index*right)) == Tree

createMap :: [String] -> Map
createMap = map repeatingAreas
	where
		repeatingAreas = cycle . (map parseCharToSpace)

removeAtModN n map = reverse $ remove map 0 []
	where
		remove [] _ acc = acc
		remove (x:xs) i acc = if i == 0 then remove xs ((i+1) `mod` n) (x:acc) else remove xs ((i+1) `mod` n) acc

parseCharToSpace '.' = Open
parseCharToSpace '#' = Tree
