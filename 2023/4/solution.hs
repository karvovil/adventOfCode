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

updateCards :: [Int] -> [Int] -> Int -> [Int]
updateCards (w:ws) cards 200 = cards
updateCards (w:ws) cards n   = updateCards ws updated (n+1)
  where updated = zipWith (+) [if i > n && i <= n+w then cards !! n else 0 | i <- [0..200]] cards

main :: IO ()
main = do
  cards <- lines <$> readFile "input.txt"

  let winNums = map (\str ->  take 10 (parseCard str)) cards
  let myNums = map parseCard (map (\line -> dropWhile (/= '|') line ++ [' '] ) cards)

  let intersection = zipWith intersect myNums winNums
  let wins =  (map length intersection)
  let points = map (\i -> 2^(i-1)) (filter (>0) wins)
  print $ sum points
  --26218

  let cards = [1 | i <- [0..200]]
  print $ sum $ updateCards wins cards 0