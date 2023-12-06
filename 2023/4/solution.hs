import Data.List
import Data.Char

parseCard card = parseCard' card []

parseCard' :: String -> [Int] -> [Int]
parseCard' (' ':c1:c2:' ':cs) rslt
  | isDigit c2                      = parseCard' (' ':cs) (rslt ++ [read (c1:c2:[]) :: Int]) 
  | otherwise                       = parseCard' (' ':cs)  rslt    
parseCard' (' ':'|':cs)       rslt = rslt
parseCard' []                 rslt = rslt
parseCard' (_:cs)             rslt = parseCard' cs rslt

main :: IO ()
main = do
  cards <- lines <$> readFile "input.txt"

  let winNums = map (\str ->  take 10 (parseCard str)) cards
  let myNums = map parseCard (map (\line -> dropWhile (/= '|') line ++ [' '] ) cards)

  let intersection = zipWith intersect myNums winNums
  let wins = filter (>0) (map length intersection)
  let points = map (\i -> 2^(i-1)) wins
  print $ sum points
  --26218