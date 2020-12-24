import Prelude hiding (Left, Right)

data Instruction = North Int | East Int | West Int | South Int | Left Int | Right Int | Foward Int deriving(Show, Read, Eq)
data Direction = N | E | W | S deriving(Show, Read, Eq)
type Position = (Int, Int)
type Waypoint = (Int, Int)

main = do 
    input <- map parseInstruction . lines <$> readFile "input.txt"
    print . sumCoordinates . foldl (\acc x -> updatePosition acc x) ((0, 0), (10, -1)) $ input

sumCoordinates ((x, y), _) = abs x + abs y

updatePosition :: (Position, Waypoint) -> Instruction -> (Position, Waypoint)
updatePosition (position , (east, south)) (North value) = (position, (east, south - value))
updatePosition (position , (east, south)) (South value) = (position, (east, south + value))
updatePosition (position , (east, south)) (East value) = (position, (east + value, south))
updatePosition (position , (east, south)) (West value) = (position, (east - value, south))
updatePosition ((east, south) , waypoint) (Foward value) = ((east + (fst waypoint * value), (south + (snd waypoint * value))), waypoint)
updatePosition (position , waypoint) instruction = (position, rotateWaypoint waypoint instruction)


rotateWaypoint :: Waypoint -> Instruction -> Waypoint
rotateWaypoint (east, south) instruction =
    case nbOfTurns `mod` 4 of
        0 -> (east, south)
        1 -> (south, -east)
        2 -> (-east, -south)
        3 -> (-south, east)
    where
        nbOfTurns = case instruction of 
            Left value -> (value `div` 90) `mod` 4
            Right value -> 4 - ((value `div` 90) `mod` 4)

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