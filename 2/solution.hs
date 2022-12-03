import Data.List

data Hand = Rock | Paper | Scissors
  deriving (Show,Eq)

data Result = Lose | Draw | Win
  deriving (Show,Eq)

charToHand :: Char -> Hand
charToHand c
  | c == 'A' || c == 'X' = Rock 
  | c == 'B' || c == 'Y' = Paper 
  | c == 'C' || c == 'Z' = Scissors 

charToResult :: Char -> Result
charToResult 'X'  = Lose
charToResult 'Y'  = Draw
charToResult 'Z'  = Win

charToPoints :: Char -> Int
charToPoints 'X'  = 0
charToPoints 'Y'  = 3 
charToPoints 'Z'  = 6 

resultToPoints :: Result -> Int
resultToPoints Lose  = 0
resultToPoints Draw  = 3 
resultToPoints Win   = 6 

handToPoints :: Hand -> Int
handToPoints Rock     = 1
handToPoints Paper    = 2
handToPoints Scissors = 3

gameScore :: Hand -> Hand -> Int 
gameScore enemy me = case me of
  Rock     -> if enemy == Paper    then 1 else if enemy == Rock     then 1+3 else 6+1
  Paper    -> if enemy == Scissors then 2 else if enemy == Paper    then 2+3 else 6+2
  Scissors -> if enemy == Rock     then 3 else if enemy == Scissors then 3+3 else 6+3

gameChoice :: Hand -> Result -> Hand
gameChoice h r = case h of
  Rock     -> if r == Draw then Rock     else if r == Lose then Scissors else Paper
  Paper    -> if r == Draw then Paper    else if r == Lose then Rock     else Scissors
  Scissors -> if r == Draw then Scissors else if r == Lose then Paper    else Rock

lineToScore :: String -> Int
lineToScore s = gameScore (charToHand $ head s) (charToHand $ head $ reverse s) 

lineToChoice :: String -> Hand
lineToChoice s = gameChoice (charToHand $ head s) (charToResult $ head $ reverse s)

lineToResults :: String -> Result
lineToResults s = charToResult $ head $ reverse s

main :: IO ()
main = do
  filecontent <- readFile "input.txt"
  let input = lines filecontent

  print $ sum $ map lineToScore input

  let choices = map lineToChoice input
  let results = map lineToResults input

  let s1 = sum $ map handToPoints choices
  let s2 = sum $ map resultToPoints results
  print (s1+s2)