import Data.List
import Data.Char
import qualified Data.Text as T


main :: IO ()
main = do
  input <- readFile "input.txt"

  let digits = (filter isDigit) <$> (lines input) 
  let firstDigits = (head <$> digits)
  let lastDigits  = (head . reverse) <$> digits
  let values = zipWith (\a b -> a:b:[]) firstDigits lastDigits
  let ints = (\v -> read v :: Int) <$> values
  print $ sum ints
