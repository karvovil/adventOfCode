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
  print cards

  let winNums = map parseCard cards
  let myNums = map parseCard (map (\line -> dropWhile (\c -> c /= '|') line ++ [' '] ) cards)

  print winNums
  print myNums