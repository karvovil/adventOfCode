import Data.Char
import Data.List

data State = State { rockN :: Int, height :: Int, moves :: Int, jetStream :: [Char], floorCoords ::  [(Int,Int)] }

instance Show State where
  show (State n height i ds floor) = "Rocks: " ++ show n ++ "  Height: " ++ show height ++ "  Moves: " ++ show i ++ "   " 

spawn :: String -> Int -> [(Int,Int)]
spawn "minus"  i = [(-1, i+4),(0 , i+4),(1 , i+4),(2 , i+4)]
spawn "plus"   i = [(0 , i+4),(0 , i+5),(0 , i+6),(-1, i+5),(1, i+5)]
spawn "corner" i = [(-1, i+4),(0 , i+4),(1 , i+4),(1 , i+5),(1, i+6)]
spawn "long"   i = [(-1, i+4),(-1, i+5),(-1, i+6),(-1, i+7)]
spawn "square" i = [(-1, i+4),(0 , i+4),(-1, i+5),(0 , i+5)]

moveRockX :: State -> [(Int,Int)] -> [(Int,Int)]
moveRockX (State n height i (d:ds) floor) rock
  | d == '<' = if any (\(x,y) -> x <= -3 || elem (x-1,y) floor) rock then rock else map (\(x,y) -> (x-1, y)) rock 
  | d == '>' = if any (\(x,y) -> x >= 3  || elem (x+1,y) floor) rock then rock else map (\(x,y) -> (x+1, y)) rock

moveRockY :: State -> [(Int,Int)] -> [(Int,Int)]
moveRockY (State n height i ds floor) rock = if any (\(x,y) -> elem (x,y-1) floor) rock then rock else map (\(x,y) -> (x,y-1)) rock

moveToRest :: Char -> State -> [(Int,Int)] -> State
moveToRest 'X' (State n height i (c:cs) floor) currentR = moveToRest 'Y' (State n height (i+1) (cs ++ [c]) floor) movedX
  where movedX = moveRockX (State n height i (c:cs) floor) currentR
moveToRest 'Y' (State n height i (c:cs) floor) currentR = if any (\(x,y) -> elem (x,y-1) floor) currentR 
                                                           then State (n+1) newHeight i (c:cs) (currentR ++ (take 100 floor))
                                                            else moveToRest 'X' (State n height i (c:cs) floor) movedY
  where movedY = moveRockY (State n height i (c:cs) floor) currentR
        newHeight = if (maximum $ map snd currentR) < height then height else maximum $ map snd currentR

moveManyRocks :: Int -> Int -> State -> State
moveManyRocks start end (State n height i cs rs)
  | start == end = (State n height i cs rs)
  | mod start 5 == 0 = moveManyRocks (start+1) end ( moveToRest 'X' (State n height i cs rs) (spawn "minus"  height) )
  | mod start 5 == 1 = moveManyRocks (start+1) end ( moveToRest 'X' (State n height i cs rs) (spawn "plus"   height) )
  | mod start 5 == 2 = moveManyRocks (start+1) end ( moveToRest 'X' (State n height i cs rs) (spawn "corner" height) )
  | mod start 5 == 3 = moveManyRocks (start+1) end ( moveToRest 'X' (State n height i cs rs) (spawn "long"   height) )
  | mod start 5 == 4 = moveManyRocks (start+1) end ( moveToRest 'X' (State n height i cs rs) (spawn "square" height) )

findRepeatingStates :: [State] -> (State,State)
findRepeatingStates ((State n h i cs rs):rest) = case result of
  Nothing     -> findRepeatingStates rest
  Just result -> ( (State n h i cs rs) , result )
  where result = find (\s -> (mod i 10091) == (mod $ moves s) 10091) (take 50 (drop 1700 rest))

main :: IO ()
main = do
  jet <- readFile "input.txt"

  let startFloor = [(-3,0),(-2,0),(-1,0),(0,0),(1,0),(2,0),(3,0)]
  print $ height $ moveManyRocks 0 2022 (State 0 0 0 jet startFloor)

  let states3500 = [ moveManyRocks 0 i (State 0 0 0 jet startFloor) | i <- [200..3500] ]

  let ss = findRepeatingStates states3500
  let hIncrease = (height $ snd ss) - (height $ fst ss)
  let rIncrease = (rockN $ snd ss) - (rockN $ fst ss)

  let bigPart   = ((1000000000000 - (rockN $ fst ss)) `div`  rIncrease) * hIncrease
  let smallPart = height $ moveManyRocks 0 (mod 1000000000000 rIncrease) (State 0 0 0 jet startFloor)
  print $ bigPart + smallPart