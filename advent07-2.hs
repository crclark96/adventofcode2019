import Data.Map.Lazy as M
import Data.List (permutations, and)

data State = State { mem :: M.Map Int Int,
                     ip :: Int,
                     halted :: Bool,
                     input :: [ Int ],
                     output :: [ Int ] } deriving Show

run :: State -> State
run s
  | halted s == True = s
  | opcode == 3 && input s == [] = s
  | otherwise = run $ runop s
    where opcode = (mem s) ! (ip s) `mod` 100

runop :: State -> State
runop (State mem ip halted input output)
  | opcode == 99 = (State mem ip True input output)
  | opcode == 1 = State { mem = M.insert dst (val1 + val2) mem,
                          ip = (ip+4),
                          halted = False,
                          input = input,
                          output = output }
  | opcode == 2 = State { mem = M.insert dst (val1 * val2) mem,
                          ip = (ip+4),
                          halted = False,
                          input = input,
                          output = output }
  | opcode == 3 = State { mem = M.insert src1 (head input) mem,
                          ip = (ip+2),
                          halted = False,
                          input = tail input,
                          output = output }
  | opcode == 4 = State { mem = mem,
                          ip = (ip+2),
                          halted = False,
                          input = input,
                          output = (val1:output) }
  | opcode == 5 = State { mem = mem,
                          ip = if val1 /= 0 then val2 else (ip+3),
                          halted = False,
                          input = input,
                          output = output }
  | opcode == 6 = State { mem = mem,
                          ip = if val1 == 0 then val2 else (ip+3),
                          halted = False,
                          input = input,
                          output = output }
  | opcode == 7 = State { mem = M.insert dst (if val1 < val2 then 1 else 0) mem,
                          ip = (ip+4),
                          halted = False,
                          input = input,
                          output = output }
  | opcode == 8 = State { mem = M.insert dst (if val1 == val2 then 1 else 0) mem,
                          ip = (ip+4),
                          halted = False,
                          input = input,
                          output = output }
  | otherwise = error "invalid opcode"
    where opcode = mem ! ip `mod` 100
          mode1  = mem ! ip `mod` 1000  `div` 100
          mode2  = mem ! ip `mod` 10000 `div` 1000
          src1   = mem ! (ip+1)
          src2   = mem ! (ip+2)
          dst    = mem ! (ip+3)
          val1   = if mode1 == 1 && opcode `elem` [1,2,5,6,7,8] then src1 else (mem ! src1)
          val2   = if mode2 == 1 && opcode `elem` [1,2,5,6,7,8] then src2 else (mem ! src2)

addInput :: Int -> State -> State
addInput i (State mem ip halted input output) = State { mem = mem,
                                                        ip = ip,
                                                        halted = halted,
                                                        input = input++[i],
                                                        output = output }

getOutput :: State -> Int
getOutput s = head $ output s

newState :: M.Map Int Int -> State
newState prog = (State prog 0 False [] [])

runAmp :: Int -> M.Map Int State -> M.Map Int State
runAmp i amps = moveSignal i $ M.insert i (run (amps ! i)) amps

moveSignal :: Int -> M.Map Int State -> M.Map Int State
moveSignal i amps = M.insert next (addInput o (amps ! next)) amps
  where o = getOutput (amps ! i)
        next = (i+1) `mod` 5

runAmps :: Int -> M.Map Int State -> Int
runAmps i amps
  | allHalted = getOutput (amps ! 4)
  | otherwise = runAmps n newAmps
  where allHalted = and . M.elems $ M.map (halted) amps
        n = (i+1) `mod` 5
        newAmps = runAmp i amps

buildAmps :: [Int] -> M.Map Int Int -> M.Map Int State
buildAmps phases prog = M.insert 0 (addInput 0 (amps' ! 0)) amps'
  where amps' = M.fromList $ zip [0..] $ zipWith (\i state -> addInput i state) phases blankamps
        blankamps = Prelude.map newState $ replicate 5 prog

main = do
  contents <- getContents
  let prog = M.fromList $ zip [0..] $ Prelude.map (read :: String -> Int) w
              where w = words [if x == ',' then ' ' else x | x <- contents]
      amps' = M.fromList $ zip [0..] $ zipWith (\i state -> addInput i state) [9,7,8,5,6] blankamps
        where blankamps = Prelude.map newState $ replicate 5 prog
      amps = M.insert 0 (addInput 0 (amps' ! 0)) amps'
      inputs = permutations [5..9]
      tests = Prelude.map (\x -> buildAmps x prog) inputs
      results = Prelude.map (runAmps 0) tests
  print $ maximum results
