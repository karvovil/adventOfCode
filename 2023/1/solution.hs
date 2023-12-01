import Data.List
import Data.Char

rplce :: String -> String
rplce [] = []
rplce ('o':'n':'e':cs) =         '1' : rplce ('e':cs)
rplce ('t':'w':'o':cs) =         '2' : rplce ('o':cs)
rplce ('t':'h':'r':'e':'e':cs) = '3' : rplce ('e':cs)
rplce ('f':'o':'u':'r':cs) =     '4' : rplce cs
rplce ('f':'i':'v':'e':cs) =     '5' : rplce ('e':cs)
rplce ('s':'i':'x':cs) =         '6' : rplce cs
rplce ('s':'e':'v':'e':'n':cs) = '7' : rplce ('n':cs)
rplce ('e':'i':'g':'h':'t':cs) = '8' : rplce ('t':cs)
rplce ('n':'i':'n':'e':cs) =     '9' : rplce ('e':cs)
rplce (c:cs) = c : rplce cs

main :: IO ()
main = do
  input <- readFile "input.txt"

  let digits = (filter isDigit) <$> (lines input) 
  let firstDigits = (head <$> digits)
  let lastDigits  = (head . reverse) <$> digits
  let values = zipWith (\a b -> a:b:[]) firstDigits lastDigits
  let ints = (\v -> read v :: Int) <$> values
  print $ sum ints

  let input2 = rplce input
  let digits2 = (filter isDigit) <$> (lines input2) 
  let firstDigits2 = (head <$> digits2)
  let lastDigits2  = (head . reverse) <$> digits2
  let values2 = zipWith (\a b -> a:b:[]) firstDigits2 lastDigits2
  let ints2 = (\v -> read v :: Int) <$> values2
  print $ sum ints2