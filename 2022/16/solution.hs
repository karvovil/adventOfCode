import Data.Char
import Data.List

data Valve = Valve {name :: String, flow :: Int, leads :: [(String,Int)], isOpen :: Bool}
  deriving Show

data State = State {currentV :: String, minutesL :: Int, totalF :: Int, allVs :: [Valve]}
  deriving Show

instance Eq State where
  State currentValve1 minutesLeft1 totalFlow1 allValves1 == State currentValve2 minutesLeft2 totalFlow2 allValves2 = minutesLeft1==minutesLeft2 && totalFlow1==totalFlow2

data State2 = State2 {v1 :: String, v2 :: String, mL1 :: Int, mL2 :: Int, tF :: Int, aVs :: [Valve]}
  deriving Show

instance Ord State where
  State currentValve1 minutesLeft1 totalFlow1 allValves1 <= State currentValve2 minutesLeft2 totalFlow2 allValves2
     | totalFlow1 < totalFlow2     = True
     | totalFlow1 > totalFlow2     = False
     | otherwise = minutesLeft2 <= minutesLeft1

instance Eq State2 where
  State2 currentValveA1 currentValveB1 minutesLeft1 ml1 totalFlow1 allValves1 == State2 currentValveA2 currentValveB2 minutesLeft2 ml2 totalFlow2 allValves2 = minutesLeft1==minutesLeft2 && totalFlow1==totalFlow2

instance Ord State2 where
  State2 currentValveA1 currentValveB1 minutesLeft1 ml1 totalFlow1 allValves1 <= State2 currentValveA2 currentValveB2 minutesLeft2 ml2 totalFlow2 allValves2
     | 50 * (minutesLeft1 + ml1) + totalFlow1 < 50 * (minutesLeft2 + ml2) + totalFlow2 = True
     | 50 * (minutesLeft1 + ml1) + totalFlow1 > 50 * (minutesLeft2 + ml2) + totalFlow2 = False
     | otherwise = minutesLeft2 <= minutesLeft1

parseFlows :: [String] -> [Int]
parseFlows []               = []
parseFlows (('e':'=':x):xs) = (read (takeWhile isDigit x) :: Int) : (parseFlows xs)
parseFlows ((c:x):xs)           = parseFlows (x:xs)

parseNames :: [String] -> [String]
parseNames []                               = []
parseNames (('V':'a':'l':'v':'e':' ':x):xs) = (take 2 x) : (parseNames xs) 

parseLeads :: String -> [(String,Int)]
parseLeads                               [] = []
parseLeads     ('v':'a':'l':'v':'e':' ':xs) = [(xs,1)]
parseLeads ('v':'a':'l':'v':'e':'s':' ':xs) = ((take 2 xs),1) : (parseLeads (drop 2 xs))
parseLeads                     (',':' ':xs) = ((take 2 xs),1) : (parseLeads (drop 2 xs)) 
parseLeads (x:xs)                           = parseLeads xs

findValve :: String -> [Valve] -> Valve
findValve s (v:vs) = if name v == s then v else findValve s vs

updateLeads :: [(String,Int)] -> String -> [(String,Int)] -> [(String,Int)]
updateLeads []        nameToUpdate newLeads = []
updateLeads (l:leads) nameToUpdate newLeads
  | fst l == nameToUpdate = (plusOne newLeads l) ++ (updateLeads leads nameToUpdate newLeads)
  | otherwise             = l : (updateLeads leads nameToUpdate newLeads)
  where plusOne lds ld = map (\(name, distance) -> (name, distance + (snd ld))) lds

removeDuplicates :: [(String,Int)] -> [(String,Int)]
removeDuplicates []              = []
removeDuplicates (l:leads) 
  | elem (fst l) (map fst leads) = removeDuplicates (filter (\ld -> fst ld /= fst l || snd ld > snd l) (l:leads))
  | otherwise                    = l : (removeDuplicates leads)

updateValves :: [Valve] -> String -> [(String,Int)] -> [Valve]
updateValves []                       nameToUpdate newLeads = []
updateValves ((Valve nme flw lds opn):vs) nameToUpdate newLeads
  | elem nameToUpdate (map fst lds) = (Valve nme flw refreshedLeads opn) : (updateValves vs nameToUpdate newLeads)
  | otherwise                       = (Valve nme flw lds            opn) : (updateValves vs nameToUpdate newLeads)
  where refreshedLeads = removeDuplicates (updateLeads lds nameToUpdate (filter (\l -> fst l /= nme ) newLeads))

trimGraph :: Int -> [Valve] -> [Valve]
trimGraph 0    vs  = vs
trimGraph n (v:vs)
  | name v == "AA" = trimGraph (n-1) (vs ++ [v])
  | flow v == 0    = trimGraph (n-1) (updateValves vs (name v) (leads v))
  | otherwise      = trimGraph (n-1) (vs ++ [v])

nextStates :: State -> [State]
nextStates (State currentValve minutesLeft totalflow allValves)
  | isOpen $ findValve currentValve allValves       = map (\l -> State (fst l) (minutesLeft-(snd l)) totalflow allValves) (leads $ findValve currentValve allValves)
  | not $ isOpen $ findValve currentValve allValves = (State currentValve (minutesLeft-1) (totalflow + (minutesLeft-1)* flow (findValve currentValve allValves)) (openValve currentValve allValves))
                                                   : (map (\l -> State (fst l) (minutesLeft-(snd l)) totalflow allValves) (leads $ findValve currentValve allValves))
        where openValve n ((Valve nme flw lds isOpn):vs) = if n == nme then (Valve nme flw lds True) : vs else (Valve nme flw lds isOpn) : (openValve n vs)

leafsN :: Int -> State -> [State]
leafsN n currentState 
  | (filter (>=n) (map minutesL (nextStates currentState))) == [] = [currentState]
  | n /= 0                     = concatMap (leafsN n) (nextStates currentState)
  | otherwise                  = concatMap (leafsN n) (filter (\ s -> minutesL s >= n) (nextStates currentState))

nextStates2 :: State2 -> [State2]
nextStates2 (State2 currentValve currentValve2 minutesLeft minutesLeft2 totalflow allValves)
  | minutesLeft >= minutesLeft2 = map (toState2 currentValve2 minutesLeft2) (nextStates (State currentValve  minutesLeft  totalflow allValves))
  | otherwise                   = map (toState2 currentValve  minutesLeft ) (nextStates (State currentValve2 minutesLeft2 totalflow allValves))
    where toState2 cv2 ml2 (State currentValve minutesLeft totalflow allValves) = State2 currentValve cv2 minutesLeft ml2 totalflow allValves

leafsN2 :: Int -> State2 -> [State2]
leafsN2 n currentState 
  | (filter (\s -> ((mL1 s) >= n) && ((mL2 s) >= n) ) (nextStates2 currentState)) == [] = [currentState]
  | n /= 0                     = concatMap (leafsN2 n) (nextStates2 currentState)
  | otherwise                  = concatMap (leafsN2 n) (filter (\ s -> (mL1 s) >= n && (mL2 s) >= n) (nextStates2 currentState))

main :: IO ()
main = do
  filecontent <- readFile "input.txt"
  let input = lines filecontent
  let allValves = zipWith4 Valve (parseNames input) (parseFlows input) (map parseLeads input) [False | i <- [1..58]]
  let trimmedValves = trimGraph 58 allValves

  let startState = State "AA" 30 0 trimmedValves
  let bestAfter15 = take 10 (reverse $ sort(leafsN 15 startState))
  print $ totalF $ maximum $ reverse $ sort (concatMap (leafsN 0) bestAfter15)

  let startState2 = State2 "AA" "AA" 26 26 0 trimmedValves
  let bestAfter10 = take 100 (reverse $ sort(leafsN2 16 startState2))
  let bestAfter18 = take 100 (reverse $ sort(concatMap (leafsN2 8) bestAfter10 ))
  print $ maximum (map tF (concatMap (leafsN2 0) bestAfter18))