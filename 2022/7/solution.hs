import Data.Char

data Directory = Directory { name :: String, directories :: [Directory], current :: Bool, fileSum :: Int }
  deriving Show

cd :: String -> Directory -> Directory
cd toName (Directory name dirs False fs) = Directory name (map (cd         toName) dirs) False fs
cd toName (Directory name dirs True  fs) = Directory name (map (setCurrent toName) dirs) False fs
  where setCurrent nme (Directory n ds crnt f) = if nme == n then (Directory n ds True f) else (Directory n ds crnt f)
                                                      
addDir :: String -> Directory -> Directory
addDir dirName (Directory name dirs False fs) = Directory name (map (addDir dirName) dirs) False fs
addDir dirName (Directory name dirs True fs ) = Directory name ((Directory dirName [] False 0):dirs) True fs

moveUp :: Directory -> Directory
moveUp (Directory name dirs crrnt fs)
  | null $ filter current dirs = Directory name (map moveUp dirs) False fs
  | otherwise                  = Directory name (map setFal dirs) True fs
      where setFal (Directory n ds c fs) = Directory n ds False fs

increment :: Int -> Directory -> Directory
increment i (Directory name dirs False fs) = Directory name (map (increment i) dirs) False fs
increment i (Directory name dirs True  fs) = Directory name dirs True (fs+i)

mapDirectory :: [String] -> Directory -> Directory
mapDirectory []                       dir = dir
mapDirectory (x:xs) dir
  | take 6 x == "$ cd ."    = mapDirectory xs (moveUp dir)
  | take 5 x == "$ cd "     = mapDirectory xs (cd (drop 5 x) dir)
  | take 4 x == "dir "      = mapDirectory xs (addDir (drop 4 x) dir)
  | isDigit (head x)        = mapDirectory xs (increment (read (takeWhile isDigit x) :: Int) dir)
  | x == "$ ls"             = mapDirectory xs dir

count :: Directory -> Int
count (Directory name []   crrnt fs) = fs 
count (Directory name dirs crrnt fs) = fs + (sum $ map count dirs) 

solve :: Directory -> Int
solve (Directory name dirs crrnt fs)
  | (count (Directory name dirs crrnt fs)) <= 100000 = (count (Directory name dirs crrnt fs)) + sum (map solve dirs)
  | (count (Directory name dirs crrnt fs)) > 100000  = sum (map solve dirs)

dirSizes :: Directory -> [Int]
dirSizes d = (count d) : concatMap dirSizes (directories d) 

findClosest :: Int -> Int -> [Int] -> Int
findClosest n best []     = best  
findClosest n best (x:xs) = if x - n >= 0 && (x - n) < (best - n) 
                              then findClosest n x xs else findClosest n best xs

main :: IO ()
main = do
  filecontent <- readFile "input.txt"
  let input = lines filecontent

  let root = Directory "/" [] True 0
  let mappedRoot = mapDirectory (tail input) root
  let spaceNeeded = (count mappedRoot) - 40000000

  print $ solve mappedRoot

  print $ findClosest spaceNeeded 9999999999 (dirSizes mappedRoot)
