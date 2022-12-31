import Data.Char
import Data.List

data State = State { moves :: Int, jetStream :: [Char], coords :: [(Int,Int)] }
  deriving Show

minus :: Int -> [(Int,Int)]
minus i = [(-1,i+4),(0,i+4),(1,i+4),(2,i+4)]

plus :: Int -> [(Int,Int)]
plus i = [(0,i+4),(0,i+5),(0,i+6),(-1,i+5),(1,i+5)]

corner :: Int -> [(Int,Int)]
corner i = [(-1,i+4),(0,i+4),(1,i+4),(1,i+5),(1,i+6)]

long :: Int -> [(Int,Int)]
long i = [(-1,i+4),(-1,i+5),(-1,i+6),(-1,i+7)]

square :: Int -> [(Int,Int)]
square i = [(-1,i+4),(0,i+4),(-1,i+5),(0,i+5)]

moveRockX :: State -> [(Int,Int)] -> [(Int,Int)]
moveRockX (State i (d:ds) allRocks) rock
  | d == '<' = if any (\(x,y) -> x <= -3 || elem (x-1,y) allRocks) rock then rock else map (\(x,y) -> (x-1, y)) rock 
  | d == '>' = if any (\(x,y) -> x >= 3  || elem (x+1,y) allRocks) rock then rock else map (\(x,y) -> (x+1, y)) rock

moveRockY :: State -> [(Int,Int)] -> [(Int,Int)]
moveRockY (State i ds allRocks) rock = if any (\(x,y) -> elem (x,y-1) allRocks) rock then rock else map (\(x,y) -> (x,y-1)) rock

moveToRest :: Char -> State -> [(Int,Int)] -> State
moveToRest 'X' (State i (c:cs) allRocks) currentRock = moveToRest 'Y' (State (i+1) (cs ++ [c]) allRocks) movedX
  where movedX = moveRockX (State i (c:cs) allRocks) currentRock
moveToRest 'Y' (State i (c:cs) allRocks) currentRock = if any (\(x,y) -> elem (x,y-1) allRocks) currentRock 
                                                      then State i (c:cs) (currentRock ++ allRocks)
                                                       else moveToRest 'X' (State i (c:cs) allRocks) movedY
  where movedY = moveRockY (State i (c:cs) allRocks) currentRock

moveManyRocks :: Int -> Int -> State -> State
moveManyRocks start end (State i cs rs)
  | start == end = (State i cs rs)
  | mod start 5 == 0 = moveManyRocks (start+1) end ( moveToRest 'X' (State i cs rs) (minus  (maximum $ map snd rs)) )
  | mod start 5 == 1 = moveManyRocks (start+1) end ( moveToRest 'X' (State i cs rs) (plus   (maximum $ map snd rs)) )
  | mod start 5 == 2 = moveManyRocks (start+1) end ( moveToRest 'X' (State i cs rs) (corner (maximum $ map snd rs)) )
  | mod start 5 == 3 = moveManyRocks (start+1) end ( moveToRest 'X' (State i cs rs) (long   (maximum $ map snd rs)) )
  | mod start 5 == 4 = moveManyRocks (start+1) end ( moveToRest 'X' (State i cs rs) (square (maximum $ map snd rs)) )

main :: IO ()
main = do
  jet <- readFile "input.txt"
  --print $ (2*) $ length jet

  let floor = [(-3,0),(-2,0),(-1,0),(0,0),(1,0),(2,0),(3,0)]
  let endState = moveManyRocks 0 3438 (State 0 jet floor)
  print $ maximum $ map snd (coords endState)
  --print $ moves endState