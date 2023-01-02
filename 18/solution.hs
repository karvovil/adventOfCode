import qualified Data.Set as Set
import Data.Char
import Data.List

data Coord = Coord Int Int Int
  deriving (Ord , Eq)

data State = State {sides :: Int, coordinates :: Set.Set Coord}
  deriving (Show)

instance Show Coord where
  show (Coord x y z) = show x ++ "," ++ show y ++ "," ++ show z ++ ", "

parseC :: String -> Coord
parseC strng = parseC' strng []
  where parseC' []       is = Coord (head $ reverse is) (head $ tail is) (head is)
        parseC' (',':xs) is = (parseC' (dropWhile isDigit xs) ((read $ takeWhile isDigit xs :: Int) : is))
        parseC'      xs  is = (parseC' (dropWhile isDigit xs) ((read $ takeWhile isDigit xs :: Int) : is))

addCube :: State -> Coord -> State
addCube (State s cs) (Coord x y z) = State (s + 6 - ((*2) . length) (Set.intersection sides cs)) (Set.insert (Coord x y z) cs)
  where sides = Set.fromList [(Coord (x-1) y z), (Coord (x+1) y z), (Coord x (y-1) z), (Coord x (y+1) z), (Coord x y (z-1)), (Coord x y (z+1))]  

addAllCubes :: State -> [Coord] -> State
addAllCubes s []     = s
addAllCubes s (c:cs) = addAllCubes (addCube s c) cs

main :: IO ()
main = do
  filecontent <- readFile "input.txt"
  let coords =  map parseC (lines filecontent)
  print $ sides $ addAllCubes (State 0 Set.empty) coords 