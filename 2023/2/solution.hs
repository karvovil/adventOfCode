import Data.List
import Data.Char

greensMax cs = greensMax' cs 0  
greensMax' :: String -> Int -> Int
greensMax' [] rslt = rslt
greensMax' (c1:c2:' ':'g':'r':'e':'e':'n':cs) rslt = greensMax' cs (max (read (c1:c2:[]) :: Int) rslt)
greensMax' (c:cs) rslt = greensMax' cs rslt

redsMax cs = redsMax' cs 0  
redsMax' :: String -> Int -> Int
redsMax' [] rslt = rslt
redsMax' (c1:c2:' ':'r':'e':'d':cs) rslt = redsMax' cs (max (read (c1:c2:[]) :: Int) rslt)
redsMax' (c:cs) rslt = redsMax' cs rslt

bluesMax cs = bluesMax' cs 0  
bluesMax' :: String -> Int -> Int
bluesMax' [] rslt = rslt
bluesMax' (c1:c2:' ':'b':'l':'u':'e':cs) rslt = bluesMax' cs (max (read (c1:c2:[]) :: Int) rslt)
bluesMax' (c:cs) rslt = bluesMax' cs rslt

isLegal :: String -> Bool
isLegal [] = True 
isLegal (c1:c2:' ':'r':'e':'d':cs) = if (read (c1:c2:[]) :: Int) > 12 then False else isLegal cs
isLegal (c1:c2:' ':'g':'r':'e':'e':'n':cs) = if (read (c1:c2:[]) :: Int) > 13 then False else isLegal cs
isLegal (c1:c2:' ':'b':'l':'u':'e':cs) = if (read (c1:c2:[]) :: Int) > 14 then False else isLegal cs
isLegal (c:cs) = isLegal cs

gameId :: String -> Int
gameId ('G':'a':'m':'e':' ':c:':':cs)         = read (c:[]) :: Int  
gameId ('G':'a':'m':'e':' ':c1:c2:':':cs)     = read (c1:c2:[]) :: Int
gameId ('G':'a':'m':'e':' ':c1:c2:c3:':':cs)  = read (c1:c2:c3:[]) :: Int

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"

  let legalLines =  filter isLegal input
  print $ sum $ gameId <$> legalLines

  let powerSum = sum $ map (\line -> (redsMax line) * (greensMax line) * (bluesMax line)) input 
  print powerSum