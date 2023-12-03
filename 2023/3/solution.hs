import Data.List
import Data.Char

symbolIn :: (Int, Int) -> [String] -> Bool
symbolIn (x, y) schematic = elem ((schematic !! y) !! x) ['-','+','*','$','#','%','@','='] 

hasAdjacentSymbol :: (Int, Int) -> [String] -> Bool
hasAdjacentSymbol (x,y) schematic = or $ map (\c -> symbolIn c schematic ) adjacent
  where adjacent = filter (\(xc, yc) -> xc >= 0 && yc >= 0 && yc <= 139 && xc <= 139) [(x-1,y),(x+1,y),(x,y-1),(x,y+1),(x+1,y+1),(x-1,y-1),(x-1,y+1),(x+1,y-1)]

allNumCoords :: (Int, Int) -> [String] ->[(Int, Int)]
allNumCoords (x,y) schematic
  | isDigit ((schematic !! y) !! (x+1)) && isDigit ((schematic !! y) !! (x+2)) = [(x,y),(x+1,y),(x+2,y)]
  | isDigit ((schematic !! y) !! (x+1))                                        = [(x,y),(x+1,y)]
  | otherwise                                                                  = [(x,y)]

numCoordsLength :: (Int, Int) -> [String] -> Int
numCoordsLength (x,y) schematic
  | isDigit ((schematic !! y) !! (x+1)) && isDigit ((schematic !! y) !! (x+2)) = 3
  | isDigit ((schematic !! y) !! (x+1))                                        = 2
  | otherwise                                                                  = 1

numCoords :: [String] -> [[(Int,Int)]]
numCoords schematic = numCoords' schematic (0,0) []

numCoords' schematic (x, y) coords
  | y > 139                           = coords
  | x > 139                           = numCoords' schematic (0,     y+1) coords
  | isDigit ((schematic !! y) !! x)   = numCoords' schematic (x+lngth, y) coords ++ [allNumCoords (x,y) schematic]
  | otherwise                         = numCoords' schematic (x+1,     y) coords
    where lngth = numCoordsLength (x,y) schematic

isPartNumber :: [String] -> [(Int,Int)] -> Bool
isPartNumber schematic coords = any (\c -> hasAdjacentSymbol c schematic) coords 

{- parseNum :: [String] -> [(Int,Int)] -> Int
parseNum schematic coords =  -}

main :: IO ()
main = do
  schematic <- lines <$> readFile "input.txt"
  let allDigits = numCoords schematic
  let filteredDigits = filter (\coords -> isPartNumber schematic coords ) allDigits 

  print $ length allDigits
  print $ length filteredDigits