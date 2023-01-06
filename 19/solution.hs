import qualified Data.Set as Set
import Data.Char
import Data.List

parseBP :: String -> BPrint
parseBP xs = parseBP' xs []
parseBP' [] is            = BPrint (head is) (is !! 1) (is !! 2) (is !! 3) (is !! 4) (is !! 5) (is !! 6)
parseBP' xs is
  | take 6 xs == "print " = parseBP' (drop 10 xs) (is ++ [read (takeWhile isDigit (drop 6 xs)) :: Int])
  | take 6 xs == "costs " = parseBP' (drop 10 xs) (is ++ [read (takeWhile isDigit (drop 6 xs)) :: Int])
  | take 4 xs == "and "   = parseBP' (drop 8  xs) (is ++ [read (takeWhile isDigit (drop 4 xs)) :: Int])
  | otherwise             = parseBP' (tail xs) is

data BPrint = BPrint {bPId :: Int, oRo :: Int, cRo :: Int, bRo :: Int, bRc :: Int, gRo :: Int, gRb :: Int}
  deriving (Show, Eq)

data Resources = Resources {ores :: Int, clays :: Int, bsidians :: Int, geodes :: Int}
  deriving (Show, Eq)

data Robots = Robots {oreRobots :: Int, clayRobots :: Int, bsRobots :: Int, geoRobots :: Int}
  deriving (Show, Eq)

data State = State {mL :: Int, blueprint :: BPrint, resources :: Resources, robots :: Robots}
  deriving Eq
  
instance Show State where
  show s = " minute " ++ (show $ 24 - (mL s)) ++ " " ++ (show $ robots s) ++ (show $ resources s) ++ "\n" 

instance Ord State where
  State  mL1 (BPrint id1 oro1 cro1 bro1 brc1 gro1 grb1) (Resources o1 c1 b1 g1) (Robots oR1 cR1 bR1 gR1) <=
   State mL2 (BPrint id2 oro2 cro2 bro2 brc2 gro2 grb2) (Resources o2 c2 b2 g2) (Robots oR2 cR2 bR2 gR2)
    | g1+mL1*gR1 /= g2+mL2*gR2 = g1+mL1*gR1 <  g2+mL2*gR2
    | b1+mL1*bR1 /= b2+mL2*bR2 = b1+mL1*bR1 <  b2+mL2*bR2
    | c1+mL1*cR1 /= c2+mL2*cR2 = c1+mL1*cR1 <  c2+mL2*cR2
    | otherwise                = o1+mL1*oR1 <= o2+mL2*oR2

nextStates :: State -> [State]
nextStates s = nextStates' s []
nextStates' :: State -> [State] -> [State]
nextStates' (State mL (BPrint id oro cro bro brc gro grb) (Resources o c b g) (Robots oR cR bR gR)) nxtStts
 | b >= grb && o >= gro                                                      = [buildGbot]
 | notElem buildBbot nxtStts && c+cR*mL < brc*mL && o-bro >= 0 && c-brc >= 0 = nextStates' s (buildBbot : nxtStts)
 | notElem buildCbot nxtStts && b+bR*mL < grb*mL && o-cro >= 0               = nextStates' s (buildCbot : nxtStts)
 | notElem buildObot nxtStts && {- c+cR*mL < brc*mL && -}         o-oro >= 0 = nextStates' s (buildObot : nxtStts)
 | otherwise                                                                 = buildNada : nxtStts
 where bp = BPrint id oro cro bro brc gro grb
       s         = State  mL    bp (Resources o         c         b         g    ) (Robots oR    cR    bR    gR   )
       buildNada = State (mL-1) bp (Resources(o+oR    )(c+cR    )(b+bR    )(g+gR)) (Robots oR    cR    bR    gR   )
       buildObot = State (mL-1) bp (Resources(o+oR-oro)(c+cR    )(b+bR    )(g+gR)) (Robots(oR+1) cR    bR    gR   )
       buildCbot = State (mL-1) bp (Resources(o+oR-cro)(c+cR    )(b+bR    )(g+gR)) (Robots oR   (cR+1) bR    gR   )
       buildBbot = State (mL-1) bp (Resources(o+oR-bro)(c+cR-brc)(b+bR    )(g+gR)) (Robots oR    cR   (bR+1) gR   )
       buildGbot = State (mL-1) bp (Resources(o+oR-gro)(c+cR    )(b+bR-grb)(g+gR)) (Robots oR    cR    bR   (gR+1))

solve1 :: Int -> [State] -> [State]
solve1 stopAt startStates = if (mL $ head startStates) == stopAt then startStates
                             else solve1 stopAt $ concatMap nextStates startStates

main :: IO ()
main = do
  filecontent <- readFile "input.txt"
  let blueprints = map parseBP (lines filecontent)
  let startStates = map (\b -> (State 24 b (Resources 0 0 0 0) (Robots 1 0 0 0))) blueprints

  let statesAfterStep1 = map (\s -> take 100 $ reverse $ sort $ solve1 12 [s]) startStates
  print $ map head statesAfterStep1

  let statesAfterStep2 = map (\s -> take 100 $ reverse $ sort $ solve1 6 s   ) statesAfterStep1
  print $ map head statesAfterStep2

  let statesAfterStep3 = map (\s ->           reverse $ sort $ solve1 0 s   ) statesAfterStep2
  print $ map head statesAfterStep3

  let maxGeodes = [maximum $ map (geodes . resources) s | s <- statesAfterStep3]
  print maxGeodes

  let qualityLevels = zipWith (*) maxGeodes (map bPId blueprints)
  print qualityLevels
  print $ sum qualityLevels
--1557 - 2725