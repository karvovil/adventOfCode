module Main where
  
import Data.List
import Data.List.Split

parse :: String -> [[Int]]
parse input =  (map . map) (read :: String -> Int) ((splitOn "\n") <$> splitOn "\n\n" input)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let sums = sum <$> parse input
  print $ (head . reverse . sort) sums
  print $ (sum . take 3 . reverse . sort) sums