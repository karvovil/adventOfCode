import Data.Char
import Data.List

data Valve = Valve {name :: String, flow :: Int, leads :: [(String,Int)], isOpen :: Bool}
  deriving Show

data State = State {currentV :: Valve, minutesL :: Int, totalF :: Int, allVs :: [Valve]}
  deriving Show

instance Eq State where
  State currentValve1 minutesLeft1 totalFlow1 allValves1 == State currentValve2 minutesLeft2 totalFlow2 allValves2 = minutesLeft1==minutesLeft2 && totalFlow1==totalFlow2

instance Ord State where
  State currentValve1 minutesLeft1 totalFlow1 allValves1 <= State currentValve2 minutesLeft2 totalFlow2 allValves2
     | totalFlow1 < totalFlow2     = True
     | totalFlow1 > totalFlow2     = False
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
  | fst l == nameToUpdate = (plusOne newLeads) ++ (updateLeads leads nameToUpdate newLeads)
  | otherwise             = l : (updateLeads leads nameToUpdate newLeads)
  where plusOne lds = map (\(name, distance) -> (name, distance + 1)) lds

removeDuplicates :: [(String,Int)] -> [(String,Int)]
removeDuplicates []              = []
removeDuplicates (l:leads) 
  | elem (fst l) (map fst leads) = removeDuplicates (filter (\ld -> fst ld /= fst l || snd ld < snd l) (l:leads))
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
  | isOpen currentValve = map (\l -> State (findValve (fst l) allValves) (minutesLeft-(snd l)) totalflow allValves) (leads currentValve)
  | not $ isOpen currentValve = (State (open currentValve) (minutesLeft-1) (totalflow + (minutesLeft-1)* flow currentValve) (openValve (name currentValve) allValves)) :
                       (map (\l -> State (findValve (fst l) allValves) (minutesLeft-(snd l)) totalflow allValves) (leads currentValve))
  where open (Valve nme flw lds False) = (Valve nme flw lds True) 
        openValve n ((Valve nme flw lds isOpn):vs) = if n == nme then (Valve nme flw lds True) : vs else (Valve nme flw lds isOpn) : (openValve n vs)

leafsN :: Int -> State -> [State]
leafsN n currentState 
  | (filter (>n) (map minutesL (nextStates currentState))) == [] = [currentState]
--  | minutesL currentState <= n = [currentState]
  | n /= 0                     = concatMap (leafsN n) (nextStates currentState)
  | otherwise                  = concatMap (leafsN n) (filter (\ s -> minutesL s > n) (nextStates currentState))



main :: IO ()
main = do
  filecontent <- readFile "input.txt"
  let input = lines filecontent
  let allValves = zipWith4 Valve (parseNames input) (parseFlows input) (map parseLeads input) [False | i <- [1..58]]
  let trimmedValves = trimGraph 58 allValves

  let startState = State (findValve "AA" trimmedValves) 30 0 trimmedValves
  mapM_ print trimmedValves
  -- 1614 - 1758
  let bestAfter15 = take 10 (reverse $ sort(leafsN 15 startState))
  print bestAfter15
  let bestAfter30 = maximum (concatMap (leafsN 0) bestAfter15)
  print bestAfter30
{-   let startState1 = State (findValve "MJ" trimmedValves) 20 2
  let startState2 = State (findValve "AC" trimmedValves) 20 2
  let startState3 = State (findValve "OI" trimmedValves) 20 2
  let startState4 = State (findValve "OU" trimmedValves) 20 2
  let startState5 = State (findValve "KW" trimmedValves) 20 3
  print $ maximum $ leafsN trimmedValves startState1
  print $ maximum $ leafsN trimmedValves startState2
  print $ maximum $ leafsN trimmedValves startState3
  print $ maximum $ leafsN trimmedValves startState4
  print $ maximum $ leafsN trimmedValves startState5 -}
