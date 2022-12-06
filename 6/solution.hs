allUnique :: String -> Bool
allUnique []     = True
allUnique (x:xs) = if x `elem` xs then False else allUnique xs

indexOf :: String -> Int -> Int -> Int
indexOf str lngth index = if allUnique (take lngth str) then index else indexOf (tail str) lngth (index+1)

main :: IO ()
main = do
  filecontent <- readFile "input.txt"
  print $ indexOf filecontent 4 4
  print $ indexOf filecontent 14 14