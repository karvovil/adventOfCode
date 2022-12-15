import Data.Char

data State = State (Int, Int) Int
  deriving Show

legalMove :: State -> State -> [String] -> Bool
legalMove (State (x1, y1) d1) (State (x2, y2) d2) graph =
    if minimum [x1,y1,x2,y2] < 0 || max x1 x2 > 161 || max y1 y2 > 40 then False
      else ord ((graph !! y2) !! x2) - ord ((graph !! y1) !! x1) <= 1

nextStates :: State -> [State]
nextStates (State (x, y) depth) = State (x+1, y)(depth+1) :
                                  State (x-1, y)(depth+1) :
                                  State (x, y+1)(depth+1) :
                                  State (x, y-1)(depth+1) : []

legalStates :: [String] -> State -> [State] -> [State] -> [State]
legalStates graph s0 []         explored = [] 
legalStates graph s0 (s:states) explored = if legalMove s0 s graph && isNotExplored s explored
                                            then  s : legalStates graph s0 states explored
                                             else     legalStates graph s0 states explored

legalStates2 :: [String] -> State -> [State] -> [State] -> [State]
legalStates2 graph s0 []         explored = [] 
legalStates2 graph s0 (s:states) explored = if legalMove s s0 graph && isNotExplored s explored
                                            then  s : legalStates2 graph s0 states explored
                                             else     legalStates2 graph s0 states explored
isNotExplored :: State -> [State] -> Bool
isNotExplored  s          []                       = True
isNotExplored (State c d) ((State c1 d1):explored) = if c == c1 then False else isNotExplored (State c d) explored

step :: [String] -> ([State], [State]) -> ([State],[State])
step graph ((s:queue) , explored) = ( queue ++ legalStates graph s (nextStates s) explored, s:explored )

step2 :: [String] -> ([State], [State]) -> ([State],[State])
step2 graph ((s:queue) , explored) = ( queue ++ legalStates2 graph s (nextStates s) explored, s:explored )


stepMany :: [String] -> ([State], [State]) -> Int
stepMany graph ((State (137,20) depth):queue, explored) = depth 
stepMany graph ((State (x  , y) depth):queue, explored) = if isNotExplored (State (x,y) depth) explored 
                                                              then stepMany graph (step graph ((State (x, y) depth):queue, explored))
                                                                else stepMany graph (queue, explored)
stepMany2 :: [String] -> ([State], [State]) -> Int
stepMany2 graph ((State (x,y) depth):queue, explored) = if ((graph !! y) !! x) == 'a' then depth else
                                                            if isNotExplored (State (x,y) depth) explored 
                                                              then stepMany2 graph (step2 graph ((State (x, y) depth):queue, explored))
                                                                else stepMany2 graph (queue, explored)

main :: IO ()
main = do
  filecontent <- readFile "input.txt"
  let input = lines filecontent
  let newL20 = take 137 ('a' : tail (input !! 20)) ++ 'z':drop 138 (input !! 20)
  let graph = take 20 input ++ [newL20] ++ drop 21 input

  let startState = State (0, 20) 0
  let startState2 = State (137, 20) 0

  print $ stepMany graph ([startState],[]) 
  print $ stepMany2 graph ([startState2],[]) 