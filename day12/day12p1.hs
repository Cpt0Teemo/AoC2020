import Prelude hiding (Left, Right)

data Instruction = North Int | East Int | West Int | South Int | Left Int | Right Int | Foward Int deriving(Show, Read, Eq)
data Direction = N | E | W | S deriving(Show, Read, Eq)
type Position = (Direction, Int, Int)

main = do 
    input <- map parseInstruction . lines <$> readFile "input.txt"
    print . sumCoordinates . foldl (\acc x -> updatePosition acc x) (E, 0, 0) $ input

sumCoordinates (_, x, y) = abs x + abs y

updatePosition :: Position -> Instruction -> Position
updatePosition (direction, east, south) (North value) = (direction, east, south - value)
updatePosition (direction, east, south) (South value) = (direction, east, south + value)
updatePosition (direction, east, south) (East value) = (direction, east + value, south)
updatePosition (direction, east, south) (West value) = (direction, east - value, south)
updatePosition state@(direction, _, _) (Foward value) = updatePosition state $ createInstruction direction value
updatePosition (direction, east, south) turn = (getNewDirection direction turn, east, south)

getNewDirection :: Direction -> Instruction -> Direction
getNewDirection direction (Left value) = 
    case (dirVal + nbOfTurns) `mod` 4 of 0 -> N; 1 -> W; 2 -> S; 3 -> E
    where
        nbOfTurns = (value `div` 90) `mod` 4
        dirVal = case direction of N -> 0; W -> 1; S -> 2; E -> 3
getNewDirection direction (Right value) = 
    case (dirVal + nbOfTurns) `mod` 4 of 0 -> N; 1 -> E; 2 -> S; 3 -> W
    where
        nbOfTurns = (value `div` 90) `mod` 4
        dirVal = case direction of N -> 0; E -> 1; S -> 2; W -> 3

createInstruction :: Direction -> Int -> Instruction
createInstruction direction value =
    case direction of
        N -> North value
        E -> East value
        W -> West value
        S -> South value

parseInstruction :: String -> Instruction
parseInstruction ('N':value) = North (read value)
parseInstruction ('E':value) = East (read value)
parseInstruction ('W':value) = West (read value)
parseInstruction ('S':value) = South (read value)
parseInstruction ('L':value) = Left (read value)
parseInstruction ('R':value) = Right (read value)
parseInstruction ('F':value) = Foward (read value)