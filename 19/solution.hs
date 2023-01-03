import qualified Data.Set as Set
import Data.Char
import Data.List

parseBP :: String -> BPrint
parseBP xs = parseBP' xs []
parseBP' [] is               = BPrint (head is) (is !! 1) (is !! 2) (is !! 3) (is !! 4) (is !! 5) (is !! 6)
parseBP' xs is
  | take 6 xs == "print " = parseBP' (drop 10 xs) (is ++ [read (takeWhile isDigit (drop 6 xs)) :: Int])
  | take 6 xs == "costs " = parseBP' (drop 10 xs) (is ++ [read (takeWhile isDigit (drop 6 xs)) :: Int])
  | take 4 xs == "and "   = parseBP' (drop 8  xs) (is ++ [read (takeWhile isDigit (drop 4 xs)) :: Int])
  | otherwise             = parseBP' (tail xs) is

data BPrint = BPrint {bPrintId :: Int, oreRore :: Int, clayRore :: Int, bsRore :: Int, bsRclay :: Int, geoRore :: Int, geoRbs :: Int}
  deriving Show

data Resources = Resources {ores :: Int, clays :: Int, bsidians :: Int, geodes :: Int}
  deriving Show

data Robots = Robots {oreRobots :: Int, clayRobots :: Int,bsRobots :: Int, geoRobots :: Int}
  deriving Show

data State = State {minute :: Int, blueprint :: BPrint, resources :: Resources, robots :: Robots}
  deriving Show
  
instance Eq State where
  State m1 b1 re1 ro1 == State m2 b2 re2 ro2 = geodes re1 == geodes re2 && geoRobots ro1 == geoRobots ro2

instance Ord State where
  State m1 b1 re1 ro1 <= State m2 b2 re2 ro2
     | (geodes re1) + (m1 * geoRobots ro1) < (geodes re2) + (m2 * geoRobots ro2) = True
     | (geodes re1) + (m1 * geoRobots ro1) > (geodes re2) + (m2 * geoRobots ro2) = False
     | otherwise = if bsidians re1 /=0 && bsidians re2 /= 0 
                    then (bsidians re1) + (m1 * bsRobots ro1) < (bsidians re2) + (m2 * bsRobots ro2)
                      else (clays re1) + (m1 * clayRobots ro1) <= (clays re2) + (m2 * clayRobots ro2)

nextStates :: State -> [State]
nextStates (State m (BPrint id oro cro bro brc gro grb) (Resources o c b g) (Robots oR cR bR gR)) = 
  filter (\s -> (ores $ resources s) >= 0 && (clays $ resources s) >= 0 && (bsidians $ resources s) >= 0 ) (
  (State (m+1) blueprint (Resources (o+oR    ) (c+cR    ) (b+bR    ) (g+gR)) (Robots  oR     cR     bR     gR   )) :
  (State (m+1) blueprint (Resources (o+oR-oro) (c+cR    ) (b+bR    ) (g+gR)) (Robots (oR+1)  cR     bR     gR   )) :
  (State (m+1) blueprint (Resources (o+oR-cro) (c+cR    ) (b+bR    ) (g+gR)) (Robots  oR    (cR+1)  bR     gR   )) :
  (State (m+1) blueprint (Resources (o+oR-bro) (c+cR-brc) (b+bR    ) (g+gR)) (Robots  oR     cR    (bR+1)  gR   )) :
  (State (m+1) blueprint (Resources (o+oR-gro) (c+cR    ) (b+bR-grb) (g+gR)) (Robots  oR     cR     bR    (gR+1))) : [] )
  where blueprint = BPrint id oro cro bro brc gro grb

solve1 :: Int -> [State] -> [State]
solve1 endMinute startStates = if (minute $ head startStates) == endMinute
                      then {- maximum $ map (geodes . resources) -} startStates
                        else solve1 endMinute $ take 50000 $ reverse $ sort $ concatMap nextStates startStates

main :: IO ()
main = do
  filecontent <- readFile "input.txt"
  let blueprints = map parseBP (lines filecontent)
  let startStates = map (\b -> (State 0 b (Resources 0 0 0 0) (Robots 1 0 0 0))) blueprints
  let qualityLevels = [ (bPrintId $ blueprint s) * (geodes $ resources $ maximum $ solve1 24 [s]) | s <- startStates ]
  print $ sum qualityLevels
--507 - 2725