-- This is really slow. Map could be used to store sand coordinates to speed up the lookup.
import Data.Char

takeFirst :: String -> (Int,Int)
takeFirst s = (read (takeWhile isDigit s) :: Int ,
                    read (takeWhile isDigit (tail $ dropWhile isDigit s )) :: Int) 

dropFirst :: String -> String
dropFirst ('-':'>':' ':xs) = xs
dropFirst (x:xs)           = if length xs < 8 then [] else dropFirst xs

betweenCoords :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
betweenCoords (x1, y1) (x2, y2)
  | x1 == x2 && y1 == y2 = (x2, y2):[]
  | x1 < x2              = (x1,y1) : betweenCoords (x1+1, y1) (x2, y2)
  | x1 > x2              = (x1,y1) : betweenCoords (x1-1, y1) (x2, y2)
  | y1 < y2              = (x1,y1) : betweenCoords (x1, y1+1) (x2, y2)
  | y1 > y2              = (x1,y1) : betweenCoords (x1, y1-1) (x2, y2)

lineToCoords :: String -> [(Int, Int)]
lineToCoords s
  | length s < 8 = []
  | otherwise    = (betweenCoords (takeFirst s) (takeFirst $ dropFirst s)) ++ (lineToCoords $ dropFirst s)
 
printLine :: Int -> Int -> Int -> [(Int, Int)] -> [(Int, Int)] -> String
printLine y startX endX rocks sands
  | startX > endX          = []
  | elem (startX, y) rocks = '#': (printLine y (startX + 1) endX rocks sands)
  | elem (startX, y) sands = 'o': (printLine y (startX + 1) endX rocks sands)
  | otherwise              = '.': (printLine y (startX + 1) endX rocks sands)
  
printAll :: (Int,Int) -> (Int, Int) -> ([(Int,Int)] , [(Int,Int)]) -> [String] 
printAll (startX, startY) (endX, endY) (rocks, sands)
  | startY > endY = []
  | otherwise     = ((show startY) ++ (printLine startY startX endX rocks sands))
                    : (printAll (startX, startY +1) (endX, endY) (rocks, sands))

moveSand :: (Int,Int) -> [(Int, Int)] -> (Int, Int)
moveSand (x,y) blocked
  | y == 170                     =          (x  , 170)
  | not $ elem (x,y+1)   blocked = moveSand (x  , y+1) blocked
  | not $ elem (x-1,y+1) blocked = moveSand (x-1, y+1) blocked
  | not $ elem (x+1,y+1) blocked = moveSand (x+1, y+1) blocked
  | otherwise                    =          (x  , y  )

nextState :: ([(Int, Int)] , [(Int, Int)]) -> ([(Int, Int)] , [(Int, Int)])
nextState (rocks, sands) = (rocks, (moveSand (500,0) (rocks ++ sands)):sands)

findN :: Int -> ([(Int, Int)] , [(Int, Int)]) -> Int
findN n (rocks, sands) = if (snd $ head sands) == 170 then n-1 else findN (n+1) (nextState (rocks, sands))  

findN2 :: Int -> ([(Int, Int)] , [(Int, Int)]) -> Int
findN2 n (rocks, sands) = if (head sands) == (500,0) then n else findN2 (n+1) (nextState (rocks, sands))  

main :: IO ()
main = do
  filecontent <- readFile "input.txt"
  let input = lines filecontent
  let rocks = concatMap lineToCoords input

  let s1 = nextState (rocks, [])
  putStr ( unlines (printAll (435, 0) (525, 170) s1) )

  print $ findN 1 s1
  print $ findN2 1 s1