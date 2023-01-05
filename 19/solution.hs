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
  deriving (Show, Eq)

data Resources = Resources {ores :: Int, clays :: Int, bsidians :: Int, geodes :: Int}
  deriving (Show, Eq)

data Robots = Robots {oreRobots :: Int, clayRobots :: Int,bsRobots :: Int, geoRobots :: Int}
  deriving (Show, Eq)

data State = State {mL :: Int, blueprint :: BPrint, resources :: Resources, robots :: Robots}
  deriving Eq
  
instance Show State where
  show s = " minute " ++ (show $ 24 - (mL s)) ++ " " ++ (show $ robots s) ++ (show $ resources s) ++ "\n" 

instance Ord State where
  State mL1 b1 re1 ro1 <= State mL2 b2 re2 ro2
     | (geodes   re1) + (mL1 * geoRobots  ro1) /= (geodes   re2) + (mL2 * geoRobots  ro2)
     = (geodes   re1) + (mL1 * geoRobots  ro1) <  (geodes   re2) + (mL2 * geoRobots  ro2) 
     | (bsidians re1) + (mL1 * bsRobots   ro1) /= (bsidians re2) + (mL2 * bsRobots   ro2)
     = (bsidians re1) + (mL1 * bsRobots   ro1) <  (bsidians re2) + (mL2 * bsRobots   ro2) 
     | (clays    re1) + (mL1 * clayRobots ro1) /= (clays    re2) + (mL2 * clayRobots ro2)
     = (clays    re1) + (mL1 * clayRobots ro1) <= (clays    re2) + (mL2 * clayRobots ro2) 
     | otherwise
     = (ores     re1) + (mL1 * oreRobots  ro1) <  (ores     re2) + (mL2 * oreRobots  ro2)

nextStates :: State -> [State]
nextStates (State mL (BPrint id oro cro bro brc gro grb) (Resources o c b g) (Robots oR cR bR gR)) =
  if b >= grb && o >= gro then [buildGbot]
   else filter (\s -> (ores $ resources s) - oR >= 0 && (clays $ resources s) - cR >= 0 && (bsidians $ resources s) - bR >= 0) (
    buildObot : buildNada 
    if b+bR*mL < grb*mL then buildCbot : if c+cR*mL < brc*mL then (buildBbot) : else
    if c+cR*mL < brc*mL then (buildBbot) : [] else : [] )
  where bp = BPrint id oro cro bro brc gro grb
        buildObot = State (mL-1) bp (Resources (o+oR-oro) (c+cR    ) (b+bR    ) (g+gR)) (Robots (oR+1)  cR     bR     gR   )
        buildCbot = State (mL-1) bp (Resources (o+oR-cro) (c+cR    ) (b+bR    ) (g+gR)) (Robots  oR    (cR+1)  bR     gR   )
        buildBbot = State (mL-1) bp (Resources (o+oR-bro) (c+cR-brc) (b+bR    ) (g+gR)) (Robots  oR     cR    (bR+1)  gR   )
        buildGbot = State (mL-1) bp (Resources (o+oR-gro) (c+cR    ) (b+bR-grb) (g+gR)) (Robots  oR     cR     bR    (gR+1))
        buildNada = State (mL-1) bp (Resources (o+oR    ) (c+cR    ) (b+bR    ) (g+gR)) (Robots  oR     cR     bR     gR   )

nextS :: State -> State
nextS (State  mL    bp (Resources  o      c      b      g    ) (Robots oR cR bR gR)) =
       State (mL-1) bp (Resources (o+oR) (c+cR) (b+bR) (g+gR)) (Robots oR cR bR gR)

solve1 ::  Int -> [State] -> [State]
--solve1 stopAt []          = []
solve1 stopAt startStates = --if (concatMap nextStates startStates) == [] then solve1 stopAt [nextS $ head startStates] else
                             if (mL $ head startStates) == stopAt then startStates
                              else solve1 stopAt $ concatMap nextStates startStates

main :: IO ()
main = do
  filecontent <- readFile "input.txt"
  let blueprints = map parseBP (lines filecontent)
  let startStates = map (\b -> (State 24 b (Resources 0 0 0 0) (Robots 1 0 0 0))) blueprints

  let statesAfterStep1 = map (\s -> take 100 $ reverse $ sort $ solve1 10 [s]) startStates
  print $ map (take 9) statesAfterStep1

  let statesAfterStep2 = map (\s -> take 100 $ reverse $ sort $ solve1 4 s   ) statesAfterStep1
  print $ map (take 9) statesAfterStep2

  let statesAfterStep3 = map (\s ->           reverse $ sort $ solve1 0 s   ) statesAfterStep2
  print $ map (take 9) statesAfterStep3

  let maxGeodes = [maximum $ map (geodes . resources) s | s <- statesAfterStep3]
  print maxGeodes

  let qualityLevels = zipWith (*) maxGeodes (map bPrintId blueprints)
  print qualityLevels
  print $ sum qualityLevels
--507 - 2725