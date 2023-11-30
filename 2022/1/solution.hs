import Data.List

numbers :: [String] -> [Int] -> [[Int]] -> [[Int]]
numbers []     current rslt = rslt
numbers (x:xs) current rslt = if x=="" then numbers xs [] (rslt ++ [current]) else numbers xs (current ++ [read x :: Int]) rslt

main :: IO ()
main = do
  filecontent <- readFile "input.txt"
  let input = lines filecontent

  let table = numbers input [] [[]]
  let sums = map sum table

  print $ sum $ take 3 (reverse . sort $ sums)