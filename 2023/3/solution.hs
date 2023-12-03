import Data.List
import Data.Char

symbolIn :: (Int, Int) -> [String] -> Bool
symbolIn (x, y) schematic = elem ((schematic !! y) !! x) ['-','+','*','$','#','%','@','='] 

hasAdjacentSymbol :: (Int, Int) -> [String] -> Bool
hasAdjacentSymbol (x,y) schematic = or $ map (\c -> symbolIn c schematic ) adjacent
  where adjacent = filter (\(xc, yc) -> xc >= 0 && yc >= 0 && yc <= 9 && xc <= 9) [(x-1,y),(x+1,y),(x,y-1),(x,y+1),(x+1,y+1),(x-1,y-1),(x-1,y+1),(x+1,y-1)]

--TODODODODODODODOD
allNumCoords :: (Int, Int) -> [String] ->[(Int, Int)]
allNumCoords (x,y) schematic = [(x,y)]

numCoords :: [String] -> [[(Int,Int)]]
numCoords schematic = numCoords' schematic (0,0) [[]]

numCoords' schematic (x, y) coords
  | y > 9                           = coords
  | x > 9                           = numCoords' schematic (0, y+1) coords
  | isDigit ((schematic !! y) !! x) = numCoords' schematic (x+3, y) coords ++ [allNumCoords (x,y) schematic]
  | otherwise                       = numCoords' schematic (x+1, y) coords

main :: IO ()
main = do
  schematic <- lines <$> readFile "input.txt"
  print $ numCoords schematic
