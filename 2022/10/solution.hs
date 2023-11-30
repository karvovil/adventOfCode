regValues :: Int -> [String] -> [Int]
regValues regval [] = []
regValues regval (x:xs)
  | take 4 x == "noop" = regval : regValues regval xs
  | take 4 x == "addx" = regval : regval : regValues (regval + val) xs
      where val = read (drop 5 x) :: Int

every40th :: [Int] -> [Int]
every40th xs = 20 * (head $ drop 19 xs) : every40th' 60 (drop 59 xs)
  where every40th' cycle [] = []
        every40th' cycle lst = cycle * (head lst) : every40th' (cycle+40) (drop 40 lst)

draw :: Int -> [Int] -> String
draw i []       = ""
draw i (x:xs)
  | mod i 40 == 0 = '\n' : (if abs (x - i) < 2 then '#' else '.') : draw ( 1 ) xs
  | otherwise     =        (if abs (x - i) < 2 then '#' else '.') : draw (i+1) xs

main :: IO ()
main = do
  filecontent <- readFile "input.txt"
  let input = lines filecontent

  let values = regValues 1 input

  print $ sum $ every40th values
  putStr $ draw 0 values