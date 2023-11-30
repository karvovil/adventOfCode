import Data.Char(digitToInt)

visible :: (Int,Int) -> [String] -> Bool
visible (x,y) rows = xVisible x (rows !! y) || xVisible y (map (\row -> (row !! x)) rows)

xVisible :: Int -> String -> Bool
xVisible 0     xs = True
xVisible 98    xs = True
xVisible coord xs = digitToInt (xs !! coord ) > maximum (take (coord  ) (map digitToInt xs)) ||
                    digitToInt (xs !! coord ) > maximum (drop (coord+1) (map digitToInt xs))

solve :: (Int,Int) -> [String] -> [Bool]
solve (x1,99) xs = [] 
solve (99,y1) xs = solve (0, y1 + 1) xs
solve (x1,y1) xs = (visible (x1,y1) xs) : solve (x1+1 , y1) xs

rightScenic :: Char -> String -> Int
rightScenic treeHight []     = 0
rightScenic treeHight (nextTree:rightTrees)
  | digitToInt nextTree < digitToInt treeHight = 1 + rightScenic treeHight rightTrees
  | otherwise                                  = 1

scenicScore :: (Int,Int) -> [String] -> Int
scenicScore (x,y) rows = rightScenic ((rows !! y) !! x) (      drop (x+1) (rows !! y)                )
                       * rightScenic ((rows !! y) !! x) (reverse $ take x (rows !! y)                )
                       * rightScenic ((rows !! y) !! x) (      drop (y+1) (map (\r -> (r !! x)) rows))
                       * rightScenic ((rows !! y) !! x) (reverse $ take y (map (\r -> (r !! x)) rows))

solve2 :: (Int,Int) -> [String] -> [Int]
solve2 (x1,99) xs = []
solve2 (99,y1) xs = solve2 (0, y1 + 1) xs
solve2 (x1,y1) xs = (scenicScore (x1,y1) xs) : solve2 (x1+1 , y1) xs

main :: IO ()
main = do
  filecontent <- readFile "input.txt"
  let forest = lines filecontent
  print $ length $ filter id (solve (0,0) forest)
  print $ maximum $ solve2 (0,0) forest