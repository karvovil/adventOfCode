import Data.Char
import Data.List
import qualified Data.Set as Set

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

data Resources = Resources {ores :: Int, clays :: Int, bsidians :: Int, geodesAtEnd :: Int}
  deriving (Show, Eq)

data Robots = Robots {oreRobots :: Int, clayRobots :: Int, bsRobots :: Int}
  deriving (Show, Eq)

data State = State {mL :: Int, resources :: Resources, robots :: Robots}
  deriving Eq
  
instance Show State where
  show s = " minute " ++ (show $ 32 - (mL s)) ++ " " ++ (show $ robots s) ++ (show $ resources s) ++ "\n" 

instance Ord State where
  State  mL1 (Resources o1 c1 b1 g1)(Robots oR1 cR1 bR1) <= State mL2 (Resources o2 c2 b2 g2)(Robots oR2 cR2 bR2)
    | g1 /= g2 = g1 < g2
    | b1+mL1*bR1 /= b2+mL2*bR2 = b1+mL1*bR1 <  b2+mL2*bR2
    | o1+mL1*oR1 /= o2+mL2*oR2 = o1+mL1*oR1 <  o2+mL2*oR2
    | otherwise{- c1+mL1*cR1 /= c2+mL2*cR2 -}= c1+mL1*cR1 <= c2+mL2*cR2

nextStates :: BPrint -> State -> [State]
nextStates bp s = nextStates' bp s []
nextStates' :: BPrint -> State -> [State] -> [State]
nextStates' (BPrint id oro cro bro brc gro grb) (State mL (Resources o c b g) (Robots oR cR bR)) nxtStts
 | b >= grb && o >= gro                                                  = [buildGbot]
-- | o >= gro &&      bR == 0 &&                                o >= bro && c >= brc = [buildBbot]
 | notElem buildBbot nxtStts && b+bR*mL < grb*mL && o >= bro && c >= brc = nextStates' bp s (buildBbot : nxtStts)
 | notElem buildCbot nxtStts && c+cR*mL < brc*mL && o >= cro             = nextStates' bp s (buildCbot : nxtStts)
 | notElem buildObot nxtStts && o+oR*mL < gro*mL && o >= oro             = nextStates' bp s (buildObot : nxtStts)
-- | o >= bro && c >= brc && o >= oro && o >= cro && b < grb               = nxtStts
 | otherwise                                                             = buildNada : nxtStts
 where bp = BPrint id oro cro bro brc gro grb
       s         = State  mL    (Resources o         c         b         g      ) (Robots oR    cR    bR   )
       buildNada = State (mL-1) (Resources(o+oR    )(c+cR    )(b+bR    )(g     )) (Robots oR    cR    bR   )
       buildObot = State (mL-1) (Resources(o+oR-oro)(c+cR    )(b+bR    )(g     )) (Robots(oR+1) cR    bR   )
       buildCbot = State (mL-1) (Resources(o+oR-cro)(c+cR    )(b+bR    )(g     )) (Robots oR   (cR+1) bR   )
       buildBbot = State (mL-1) (Resources(o+oR-bro)(c+cR-brc)(b+bR    )(g     )) (Robots oR    cR   (bR+1))
       buildGbot = State (mL-1) (Resources(o+oR-gro)(c+cR    )(b+bR-grb)(g+mL-1)) (Robots oR    cR    bR   )

solve ::  BPrint -> Int -> [State] -> Int
solve bp best []                    = best
solve bp best (s:stack)
  | maxResult s <= best             = solve bp best          stack
  | mL s == 0 && resultNow s > best = solve bp (resultNow s) stack
--  | mL s == 0 && resultNow s <= best = solve bp best          stack
  | otherwise                       = solve bp best          ((nextStates bp s) ++ stack)   
  where resultNow state = geodesAtEnd $ resources state
        maxResult state = (resultNow state) + sum [1..(mL state)-1]

main :: IO ()
main = do
  filecontent <- readFile "input.txt"
  let blueprints = map parseBP (lines filecontent)
  let startState1 = State 24 (Resources 0 0 0 0) (Robots 1 0 0)
  let startState2 = State 32 (Resources 0 0 0 0) (Robots 1 0 0)

--  print $ sum $ map (\b -> (solve b 0 [startState1])*(bPId b)) blueprints

  print $ solve (blueprints !! 0) 0 [startState2] 
  print $ solve (blueprints !! 1) 0 [startState2] 
  print $ solve (blueprints !! 2) 0 [startState2] 
  {- 
  print $ solve (blueprints !! 3) 0 [startState1] 
  print $ solve (blueprints !! 4) 0 [startState1] 
  print $ solve (blueprints !! 5) 0 [startState1] 
  print $ solve (blueprints !! 6) 0 [startState1] 
  print $ solve (blueprints !! 7) 0 [startState1] 
  print $ solve (blueprints !! 8) 0 [startState1] 
  print $ solve (blueprints !! 9) 0 [startState1] 
  print $ solve (blueprints !! 10) 0 [startState1] 
  print $ solve (blueprints !! 11) 0 [startState1] 
  print $ solve (blueprints !! 12) 0 [startState1] 
  print $ solve (blueprints !! 13) 0 [startState1] 
  print $ solve (blueprints !! 14) 0 [startState1] 
  print $ solve (blueprints !! 15) 0 [startState1] 
  print $ solve (blueprints !! 16) 0 [startState1] 
  print $ solve (blueprints !! 17) 0 [startState1] 
  print $ solve (blueprints !! 18) 0 [startState1] 
  print $ solve (blueprints !! 19) 0 [startState1] 
  print $ solve (blueprints !! 20) 0 [startState1] 
  print $ solve (blueprints !! 21) 0 [startState1] 
  print $ solve (blueprints !! 22) 0 [startState1] 
  print $ solve (blueprints !! 23) 0 [startState1] 
  print $ solve (blueprints !! 24) 0 [startState1] 
  let maxGeodes = [maximum $ map (geodes . resources) s | s <- statesAfterStep3]
  print maxGeodes

  let qualityLevels = zipWith (*) maxGeodes (map bPId blueprints)
  print qualityLevels
  print $ sum qualityLevels -}
--1599
--13824 too low