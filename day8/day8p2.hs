import Data.List

data Command = Acc Int | Jump Int | Nop Int deriving (Eq, Show, Read)
type Commands = [(Int, Command)]
type SavedState = (Int, Int)

main :: IO()
main = do
	input <- lines <$> readFile "input.txt"
	print . run 0 0 [] (length input). indexList . map parseCommand $ input

run :: Int -> Int -> [SavedState] -> Int -> [(Int, Command)] -> Int
run counter acc savedStates lastIndex instructions =
	if counter == lastIndex then acc else
	case getCommand counter instructions of
		Just (Acc value) -> run (counter + 1) (acc + value) savedStates lastIndex nextInstructions
		Just (Jump value) -> run (counter + value) acc (savedStates++[(counter, acc)]) lastIndex nextInstructions
		Just (Nop value)  -> run (counter + 1) acc (savedStates++[(counter + value - 1, acc)]) lastIndex nextInstructions
		Nothing -> let (savedCounter, savedAcc) = head savedStates in run (savedCounter + 1) savedAcc (tail savedStates) lastIndex nextInstructions
	where
		nextInstructions = deleteCommand counter instructions


parseCommand :: String -> Command
parseCommand string 
	| name == "acc" = Acc value
	| name == "jmp" = Jump value
	| name == "nop" = Nop value
	where
		name = head . words $ string
		value = parseInt . head . reverse . words $ string

getCommand :: Int -> Commands -> Maybe Command
getCommand = lookup 

deleteCommand :: Int -> Commands  -> Commands
deleteCommand index = deleteBy (\ x y -> fst x == fst y ) (index, Nop 0)

parseInt :: String -> Int
parseInt string =
	case head string of
		'+' -> number
		'-' -> -number
	where
		number = read . tail $ string

indexList = zip [0..]
