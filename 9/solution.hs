import Data.Char
import Data.List

moveHead :: Char -> (Int,Int) -> (Int,Int)
moveHead 'L' (hx, hy) = (hx-1, hy  ) 
moveHead 'R' (hx, hy) = (hx+1, hy  )
moveHead 'U' (hx, hy) = (hx  , hy+1)
moveHead 'D' (hx, hy) = (hx  , hy-1)

moveTail :: (Int, Int) -> (Int, Int) -> (Int, Int) 
moveTail (hx,hy) (tx,ty)
  | abs (tx-hx) < 2 && abs (ty-hy) < 2 = (tx,ty)
  | tx-hx == 0                         = (tx, if ty-hy < 0 then ty+1 else ty-1)
  | ty-hy == 0                         = (if tx-hx < 0 then tx+1 else tx-1 ,ty)
  | hy > ty                            = if hx > tx then (tx+1,ty+1) else (tx-1,ty+1)
  | hy < ty                            = if hx < tx then (tx-1,ty-1) else (tx+1,ty-1)

moveMany :: ((Int,Int) , (Int,Int)) -> Char -> Int -> [((Int,Int), (Int,Int))]
moveMany (h, t) direction 0 = []
moveMany (h, t) direction n = ( newHead, newTail ) : moveMany (newHead, newTail) direction (n-1)
  where newHead = moveHead direction h
        newTail = moveTail newHead t

moveLongTail :: [(Int,Int)] -> [(Int,Int)]
moveLongTail (k1:[])      = []
moveLongTail (k1:k2:rope) = moveTail k1 k2 : moveLongTail ((moveTail k1 k2):rope)

moveLongRopeMany :: Char -> Int -> [(Int,Int)] -> [[(Int,Int)]]
moveLongRopeMany c 0 start = []
moveLongRopeMany c n start = [newHead : newTail] ++ moveLongRopeMany c (n-1) (newHead:newTail)
  where newHead = moveHead c (head start)
        newTail = moveLongTail (newHead : (tail start))

solve :: [String] -> [(Int,Int)] -> [[(Int,Int)]]
solve []     start = []
solve (x:xs) start = newCoords ++ solve xs (head $ reverse newCoords) 
  where newCoords = moveLongRopeMany (head x) (findN x) start
        findN s = read (takeWhile isDigit (drop 2 s)) :: Int

main :: IO ()
main = do
  filecontent <- readFile "input.txt"
  let input = lines filecontent

  let allCoordinates = solve input [(0,0), (0,0)]
  print $ length $ nub $ map (head . reverse) allCoordinates

  let allLong = solve input [(0,0), (0,0), (0,0), (0,0), (0,0), (0,0), (0,0), (0,0), (0,0), (0,0)]
  print $ length $ nub $ map (head . reverse) allLong
  