import Data.Char

parseItems :: [String] -> [[Int]]
parseItems []     = []
parseItems (x:xs) = if take 3 x == "  S" then (parse x) : parseItems xs else parseItems xs
  where parse []           = []
        parse (c1:c2:rest) = if isDigit c1 && isDigit c2 then (read [c1,c2] :: Int) : parse rest else parse (c2:rest)

parseOperations :: [String] -> [(Int -> Int)] 
parseOperations []                    = []
parseOperations (x:xs) = if (take 3 x) == "  O" then (parseOne x) : parseOperations xs else parseOperations xs
  where parseOne s = case (s !! 23) of
          '*' -> if isDigit (s !! 25) then (* (read (drop 24 s) :: Int)) else (^2)
          '+' -> if isDigit (s !! 25) then (+ (read (drop 24 s) :: Int)) else (*2)

parseTestValues :: [String] -> [Int]
parseTestValues []                   = []
parseTestValues ((' ':' ':'T':s):xs) = (read $ reverse $ takeWhile isDigit $ reverse s :: Int) : parseTestValues xs
parseTestValues (x:xs)               = parseTestValues xs

parseT :: [String] -> [Int]
parseT []     = []
parseT (s:xs) = if take 8 s == "    If t" then (digitToInt $ head $ reverse s) : parseT xs else parseT xs

parseF :: [String] -> [Int]
parseF []     = []
parseF (s:xs) = if take 8 s == "    If f" then (digitToInt $ head $ reverse s) : parseF xs else parseF xs

monkeyFunction :: (Int -> Int) -> Int -> Int -> Int -> Int -> Int
monkeyFunction operation testValue tMonki fMonki item = if mod (operation item) testValue == 0
                                                        then tMonki else fMonki

parseFunctions :: [String] -> [(Int -> Int)]
parseFunctions lst = zipWith ($) (zipWith ($) (zipWith ($) (zipWith ($) 
      [monkeyFunction | x <- [0..9]] (parseOperations lst)) (parseTestValues lst)) (parseT lst)) (parseF lst) 

inspect :: Int -> (Int -> Int) -> (Int -> Int) -> [[Int]] -> [[Int]]
inspect i op f allItems = take (f i) allItems ++ [((allItems !! (f i)) ++ [mod (op i) (13*7*3*19*5*2*11*17)])] ++ drop ((f i)+1) allItems

inspectAll :: Int -> [Int] -> (Int -> Int) -> (Int -> Int) -> [[Int]] -> [[Int]]
inspectAll n []     op f allItems = (take n allItems) ++ [] : (drop (n+1) allItems)
inspectAll n (i:is) op f allItems = inspectAll n is op f (inspect i op f allItems)

inspectAllMonkis :: Int -> Int -> [Int] -> [[Int]] -> [(Int -> Int)] -> [(Int -> Int)] -> [[Int]]
inspectAllMonkis 10000 8 times is os fs = is ++ [times]
inspectAllMonkis round 8 times is os fs = inspectAllMonkis (round+1) 0 times is os fs
inspectAllMonkis round n times is os fs = inspectAllMonkis round (n+1)
                                            (take n times ++ [(times!!n) + length (is!!n)] ++ drop (n+1) times) 
                                              (inspectAll n (is!!n) (os!!n) (fs!!n) is)  os fs

main :: IO ()
main = do
  filecontent <- readFile "input.txt"
  let input = lines filecontent

  let fs = parseFunctions input
  let is = parseItems input
  let ops = parseOperations input
  let endLevels = inspectAllMonkis 1 0 [0,0,0,0,0,0,0,0] is ops fs
  print endLevels
  print $ 152660 * 142807