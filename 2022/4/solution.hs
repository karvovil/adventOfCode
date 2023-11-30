import Data.List
import Data.Maybe

parseNumbers :: String -> String -> [Int] -> [Int]
parseNumbers [] num ints = ints
parseNumbers (x:[]) num ints = ints ++ [(read (num ++ [x]) :: Int)]
parseNumbers (x:xs) num ints = if x == '-' || x==',' then parseNumbers xs "" (ints ++ [(read num :: Int)]) else parseNumbers xs (num ++ [x]) ints

isContained :: [Int] -> Bool
isContained list
  | (list !! 0) <= (list !! 2) && (list !! 1) >= (list !! 3) = True
  | (list !! 0) >= (list !! 2) && (list !! 1) <= (list !! 3) = True
  | otherwise                                                = False

overLaps :: [Int] -> Bool
overLaps list
  | (list !! 1) < (list !! 2) = False
  | (list !! 3) < (list !! 0) = False
  | otherwise                 = True

main :: IO ()
main = do
  filecontent <- readFile "input.txt"
  let input = lines filecontent
  
  let numbers = map (\s -> parseNumbers s "" []) input

  print $ length $ filter isContained numbers
  print $ length $ filter overLaps numbers