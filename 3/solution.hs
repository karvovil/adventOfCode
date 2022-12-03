import Data.List
import Data.Maybe

priority :: Char -> Int
priority c = fromJust $ elemIndex c "0abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

first :: String -> String
first s = take ((length s) `div` 2) s

second :: String -> String
second s = drop ((length s) `div` 2) s

isIn :: Char -> String -> Bool
isIn c [] = False
isIn c (x:xs) = if c==x then True else isIn c xs

common :: String -> String -> Char
common [] ys = '0'
common (x:xs) ys = if isIn x ys then x else common xs ys 

divideAndFind :: String -> Char
divideAndFind s = common (first s) (second s)

common3 :: String -> String -> String -> Char
common3 []     ys zs = '0'
common3 (x:xs) ys zs = if isIn x ys && isIn x zs then x else common3 xs ys zs

solveGroup :: String -> String -> String -> Int
solveGroup s1 s2 s3 = priority $ common3 s1 s2 s3

solve :: [String] -> Int
solve [] = 0
solve sacks = solveGroup (head sacks) (head . tail $ sacks) (head . tail . tail $ sacks) + solve (drop 3 sacks) 

main :: IO ()
main = do
  filecontent <- readFile "input.txt"
  let input = lines filecontent
  
  print $ sum $ map priority (map divideAndFind input)

  print $ solve input