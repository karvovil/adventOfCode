import Data.List
import Data.Char

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