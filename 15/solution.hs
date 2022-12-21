import qualified Data.Set as Set
import Data.Char
import Data.List

numbers :: String -> [Int]
numbers []           = []
numbers ('=':'-':xs) = (-(read $ takeWhile isDigit xs :: Int)) : (numbers xs)
numbers ('=':xs)     =   (read $ takeWhile isDigit xs :: Int)  : (numbers xs)
numbers (x:xs)       =                                            numbers xs

parseCoords :: [String] -> [((Int,Int),(Int,Int))]
parseCoords []     = []
parseCoords (x:xs) = ((((numbers x)!!0),((numbers x)!!1)),
                      (((numbers x)!!2),((numbers x)!!3))) : (parseCoords xs)

notBeacon2000000 :: ((Int,Int),(Int,Int)) -> [Int]
notBeacon2000000 ((sX,sY),(bX,bY)) = if abs(sX-bX) + abs(sY-bY) < abs(sY-2000000) then []
                                      else [(sX - (((abs(sX-bX) + abs(sY-bY))) - abs(sY-2000000)))..
                                            (sX + (((abs(sX-bX) + abs(sY-bY))) - abs(sY-2000000)))]

notBeans200000 :: [((Int,Int),(Int,Int))] -> Set.Set Int -> Set.Set Int
notBeans200000 []     set = set
notBeans200000 (x:xs) set = notBeans200000 xs (Set.union set (Set.fromList(notBeacon2000000 x)))

manh :: ((Int,Int),(Int,Int)) -> Int
manh ((sX,sY),(bX,bY)) = abs(sX-bX) + abs(sY-bY)

manPlus1 :: (Int,Int) -> Int -> Char -> [(Int,Int)]
manPlus1 (sX,sY) 0   cha = (sX,sY) : []
manPlus1 (sX,sY) man cha
  | sY < 0       = []
  | sY > 4000000 = []
  | cha == '-'   = (sX - man, sY) : (sX + man, sY) : manPlus1 (sX, sY-1) (man-1) '-'
  | cha == '+'   = (sX - man, sY) : (sX + man, sY) : manPlus1 (sX, sY+1) (man-1) '+'
  | cha == '0'   = (sX - man, sY) : (sX + man, sY) : manPlus1 (sX, sY-1) (man-1) '-'
                                                  ++ manPlus1 (sX, sY+1) (man-1) '+'

canBeBeacon :: (Int,Int) -> [((Int,Int),(Int,Int))] -> Bool
canBeBeacon coord []                            = True
canBeBeacon coord ((sensor,beacon):xs)
  | fst coord < 0 || fst coord > 4000000        = False
  | snd coord < 0 || snd coord > 4000000        = False
  | manh (sensor,beacon) >= manh (sensor,coord) = False
  | manh (sensor,beacon) <  manh (sensor,coord) = canBeBeacon coord xs

main :: IO ()
main = do
  filecontent <- readFile "input.txt"
  let input = lines filecontent
  let coords = parseCoords input

  let coordSet = notBeans200000 coords Set.empty
  print $ (length coordSet) -1

  let allPlusOnes = concatMap (\(sensor,beacon) -> manPlus1 sensor (1+manh (sensor,beacon)) '0') coords
  let possibleBeacons = [ x | x <- allPlusOnes, canBeBeacon x coords ]
  
  print $ (fst $ head possibleBeacons) * 4000000 + (snd $ head possibleBeacons)