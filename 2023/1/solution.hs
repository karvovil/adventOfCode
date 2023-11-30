import Data.List

main :: IO ()
main = do
  filecontent <- readFile "input.txt"
  print filecontent