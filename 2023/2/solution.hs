import Data.List
import Data.Char

colorMax cs = colorMax' cs (0,0,0)
colorMax' :: String -> (Int,Int,Int) -> (Int,Int,Int)
colorMax' [] rslt = rslt
colorMax' (c1:c2:' ':'r':'e':'d':cs) (r,g,b)         = colorMax' cs ((max (read (c1:c2:[]) :: Int) r), g, b)
colorMax' (c1:c2:' ':'g':'r':'e':'e':'n':cs) (r,g,b) = colorMax' cs (r, (max (read (c1:c2:[]) :: Int) g), b)
colorMax' (c1:c2:' ':'b':'l':'u':'e':cs) (r,g,b)     = colorMax' cs (r, g, (max (read (c1:c2:[]) :: Int) b))
colorMax' (c:cs) rslt                                = colorMax' cs rslt

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


  let colorMaximums = map colorMax input
  print $ sum $ (\(r,g,b) -> r*g*b) <$> colorMaximums