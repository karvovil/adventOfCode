import Data.List
import Data.Char
import Data.Maybe

readLists :: [String] -> ([String],[String]) -> ([String],[String])
readLists []       (xl, yl) = (reverse xl, reverse yl)
readLists ("":xs)  (xl, yl) = readLists xs (xl ,yl)
readLists (x:y:xs) (xl, yl) = readLists xs (x:xl , y:yl)

nextItem :: String -> Int -> String
nextItem (']':xs) 1 = []
nextItem ('[':xs) 0 = nextItem xs 1
nextItem (',':xs) 0 = []
nextItem ('[':xs) n = '[' : nextItem xs (n+1)
nextItem (']':xs) n = ']' : nextItem xs (n-1)
nextItem (x:xs)   0 = if isDigit x then takeWhile isDigit (x:xs) else x: nextItem xs 0
nextItem (x:xs)   n = x:(nextItem xs n)

dropNext :: String -> Int -> String
dropNext []       0     = []
dropNext ('[':xs) 0     = dropNext xs 1
dropNext (',':xs) 0     = xs
dropNext (']':[]) 1     = []
dropNext ('[':xs) n     = dropNext xs (n+1)
dropNext (']':xs) n     = dropNext xs (n-1)
dropNext (x:xs)   0     = if isDigit x then dropNext (dropWhile isDigit (x:xs)) 0 else dropNext xs 0
dropNext (x:xs)   n     = dropNext xs n

items :: String -> [String]
items [] = []
items s  = (nextItem s 0) : (items $ dropNext s 0)

count :: [Bool] -> Int
count bs = count' bs 1
  where count' (b:bs) n = if b then n + count' bs (n+1) else count' bs (n+1)
        count' []     n = 0

isInOrder :: [String] -> [String] -> Bool
isInOrder []       []       = True
isInOrder []       xs2      = True
isInOrder xs1      []       = False
isInOrder (s1:xs1) (s2:xs2)
  | s1 == s2                                     = isInOrder xs1 xs2
  | s1 == ""                                     = True
  | s2 == ""                                     = False
  | allDigits s1         && allDigits s2         = (read s1 :: Int) < (read s2 :: Int)
  | (not $ allDigits s1) && (not $ allDigits s2) = isInOrder (items s1) (items s2)
  | allDigits s1         && (not $ allDigits s2) = isInOrder [s1] (items s2)
  | (not $ allDigits s1) && allDigits s2         = isInOrder (items s1) [s2]
  where allDigits s = length (filter isDigit s) == length s

sortFunction :: String -> String -> Ordering
sortFunction xs1 xs2
  | isInOrder (items xs1) (items xs2) == True  = GT
  | isInOrder (items xs1) (items xs2) == False = LT

main :: IO ()
main = do
  filecontent <- readFile "input.txt"
  let input = lines filecontent
  let lists = readLists input ([],[])

  let results = zipWith isInOrder (map items (fst lists)) (map items (snd lists))
  print $ count results

  let allPackets = ("[[6]]":"[[2]]":(fst lists) ++ (snd lists))
  let sortedPackets = reverse $ sortBy sortFunction allPackets
  let i1 = fromJust $ elemIndex "[[2]]" sortedPackets
  let i2 = fromJust $ elemIndex "[[6]]" sortedPackets
  print $ (1 + i1) * (i2 + 1)