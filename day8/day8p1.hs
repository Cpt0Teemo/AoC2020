import Data.List

data Command = Acc Int | Jump Int | Nop deriving (Eq, Show, Read)
type Commands = [(Int, Command)]

main :: IO()
main = do
	input <- lines <$> readFile "input.txt"
	print . run 0 0 . indexList . map parseCommand $ input

run :: Int -> Int -> [(Int, Command)] -> Int
run counter acc instructions =
	case getCommand counter instructions of
		Just (Acc value) -> run (counter + 1) (acc + value) instructions
		Just (Jump value) -> run (counter + value) acc instructions
		Just Nop  -> run (counter + 1) acc instructions
		Nothing -> acc
	where
		instructions = deleteCommand counter instructions


parseCommand :: String -> Command
parseCommand string 
	| name == "acc" = Acc value
	| name == "jmp" = Jump value
	| name == "nop" = Nop
	where
		name = head . words $ string
		value = parseInt . head . reverse . words $ string

getCommand :: Int -> Commands -> Maybe Command
getCommand = lookup 

deleteCommand :: Int -> Commands  -> Commands
deleteCommand index = deleteBy (\ x y -> fst x == fst y ) (index, Nop)

parseInt :: String -> Int
parseInt string =
	case head string of
		'+' -> number
		'-' -> -number
	where
		number = read . tail $ string

indexList = zip [0..]
