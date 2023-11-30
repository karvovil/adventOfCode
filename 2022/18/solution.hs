import qualified Data.Set as Set
import Data.Char
import Data.List

data Coord = Coord Int Int Int
  deriving (Ord , Eq)
  
instance Show Coord where
  show (Coord x y z) = show x ++ "," ++ show y ++ "," ++ show z ++ ", "

parseC :: String -> Coord
parseC strng = parseC' strng []
  where parseC' []       is = Coord (head $ reverse is) (head $ tail is) (head is)
        parseC' (',':xs) is = (parseC' (dropWhile isDigit xs) ((read $ takeWhile isDigit xs :: Int) : is))
        parseC'      xs  is = (parseC' (dropWhile isDigit xs) ((read $ takeWhile isDigit xs :: Int) : is))

nextCoords :: Coord -> Set.Set Coord 
nextCoords (Coord x y z) = Set.fromList [(Coord (x-1) y z),(Coord x (y-1) z),(Coord x y (z-1)),
                                         (Coord (x+1) y z),(Coord x (y+1) z),(Coord x y (z+1))]
                                          
addAllCubes :: Int -> Set.Set Coord -> [Coord] -> Int
addAllCubes i lava []     = i
addAllCubes i lava (c:cs) = addAllCubes newI (Set.insert c lava) cs
  where newI = (i + 6 - ((*2) . length) (Set.intersection (nextCoords c) lava))

canGetOut :: Set.Set Coord -> [Coord] -> Bool
canGetOut badCs []                              = False
canGetOut badCs ((Coord x y z):tryCs)
  | minimum [x,y,z] < 1 || maximum [x,y,z] > 18 = True
  | otherwise = canGetOut (Set.insert (Coord x y z) badCs) (nub $ nextPossibeCoordinates ++ tryCs)
  where nextPossibeCoordinates = Set.toList (Set.difference (nextCoords (Coord x y z)) badCs)

insideCoords :: Set.Set Coord -> [Coord] -> [Coord]
insideCoords lavaCs []        = []
insideCoords lavaCs (c:tryCs)
  | Set.member c lavaCs       =     insideCoords lavaCs tryCs
  | canGetOut lavaCs [c]      =     insideCoords lavaCs tryCs  
  | otherwise                 = c : insideCoords lavaCs tryCs

countSides :: Set.Set Coord -> [Coord] -> Int
countSides lavaCs [] = 0  
countSides lavaCs (c:cs) = count c + countSides lavaCs cs
  where count c = Set.size $ Set.intersection (nextCoords c) lavaCs

main :: IO ()
main = do
  filecontent <- readFile "input.txt"
  let lavaList = map parseC (lines filecontent)
  print $ addAllCubes 0 Set.empty lavaList

  let tryCoordinates = [Coord i j k | i <- [1..18], j <- [1..18], k <- [1..18]]
  let lavaSet = Set.fromList lavaList
  let airBubbles = insideCoords lavaSet tryCoordinates
  print $ (addAllCubes 0 Set.empty lavaList) - (countSides lavaSet airBubbles) 