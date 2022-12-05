import Data.List
import Data.Maybe

trimStacks :: [String] -> [String]
trimStacks []         = []
trimStacks (s:stacks) = (trimStack (reverse s)) : (trimStacks stacks)
  
trimStack :: String -> String
trimStack (x:xs) = if x /= ' ' then (x:xs) else trimStack xs

parseStacks :: [String] -> [String] -> [String]
parseStacks (" 1   2   3   4   5   6   7   8   9 ":xs) stacks = stacks
parseStacks (x:xs) stacks = parseStacks xs (parseStack x stacks)

parseMoves :: [String] -> [Int] -> [Int]
parseMoves []                    current = current
parseMoves (('m':chars):strings) current = parseMoves strings (current ++ (parseMove chars))
parseMoves (x:xs)                current = parseMoves xs current

parseMove :: String -> [Int]
parseMove ('e':' ':x:y:xs) = if y == ' ' then (read [x] :: Int)   : (parseMove xs) 
                              else            (read [x,y] :: Int) : (parseMove xs)
parseMove ('m':' ':x:xs) = (read [x] :: Int) : parseMove xs
parseMove ('o':' ':x:[]) = (read [x] :: Int) : []
parseMove (x:xs)         = parseMove xs

parseStack :: String -> [String] -> [String]
parseStack ""           stack = stack
parseStack (x:y:z:rest) stack
  | x=='[' && z==']' = y : parseStack rest stack
  | otherwise        = parseStack (y:z:rest) stack

  --[(s !! 1) :(stack !! 0),
  --                    (s !! 5) :(stack !! 1),
  --                    (s !! 9) :(stack !! 2),
  --                    (s !! 13):(stack !! 3),
  --                    (s !! 17):(stack !! 4),
  --                    (s !! 21):(stack !! 5),
  --                    (s !! 25):(stack !! 6),
  --                    (s !! 29):(stack !! 7),
  --                    (s !! 33):(stack !! 8)]


remove :: Int -> Int -> [String] -> [String]
remove n from stacks = (take (from - 1) stacks) ++ [drop n (stacks !! (from - 1))] ++ drop from stacks

add :: String -> Int -> [String] -> [String]
add x to stacks = (take (to - 1) stacks) ++ [x ++ (stacks !! (to - 1))] ++ drop to stacks

takeN :: Int -> Int -> [String] -> String
takeN n from stacks = take n (stacks !! (from - 1))

move :: Int -> Int -> Int -> [String] -> [String]
move n from to stacks = add (reverse $ takeN n from stacks) to (remove n from stacks)

moveMany :: (Int -> Int -> Int -> [String] -> [String]) -> [Int] -> [String] -> [String]
moveMany moveFunction []                   stacks = stacks
moveMany moveFunction (x:y:z:instructions) stacks = moveMany moveFunction instructions ( moveFunction x y z stacks )

move2 :: Int -> Int -> Int -> [String] -> [String]
move2 n from to stacks = add (takeN n from stacks) to (remove n from stacks)

main :: IO ()
main = do
  filecontent <- readFile "input.txt"
  let input = lines filecontent
  let stacks =  trimStacks (parseStacks input ["","","","","","","","",""])
  let instructions = parseMoves input []

  print $ map head (moveMany move instructions stacks)
  print $ map head (moveMany move2 instructions stacks)