import Data.List
import Data.Char

symbolIn :: (Int, Int) -> [String] -> Bool
symbolIn (x, y) schematic = elem ((schematic !! y) !! x) ['-','+','*','$','#','%','@','='] 

hasAdjacentSymbol :: (Int, Int) -> [String] -> Bool
hasAdjacentSymbol (x,y) schematic = or $ map (\c -> symbolIn c schematic ) adjacent
  where adjacent = filter (\(xc, yc) -> xc >= 0 && yc >= 0) [(x-1,y),(x+1,y),(x,y-1),(x,y+1),(x+1,y+1),(x-1,y-1),(x-1,y+1),(x+1,y-1)]

main :: IO ()
main = do
  schematic <- lines <$> readFile "input.txt"
  print $ hasAdjacentSymbol (1, 0) schematic