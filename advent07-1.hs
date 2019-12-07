import Data.Map.Lazy as M
import Data.List (permutations)

data State = State { mem :: M.Map Int Int,
                     input :: [ Int ],
                     output :: [ Int ] } deriving Show

runop :: Int -> State -> State
runop i (State mem input output)
  | opcode == 99 = (State mem input output)
  | opcode == 1 = runop (i+4) $ State { mem = M.insert dst (val1 + val2) mem,
                                        input = input, output = output }
  | opcode == 2 = runop (i+4) $ State { mem = M.insert dst (val1 * val2) mem,
                                        input = input, output = output }
  | opcode == 3 = runop (i+2) $ State { mem = M.insert src1 (head input) mem,
                                        input = tail input, output = output }
  | opcode == 4 = runop (i+2) $ State { mem = mem, input = input,
                                        output = (val1:output) }
  | opcode == 5 = runop (if val1 /= 0 then val2 else (i+3)) (State mem input output)
  | opcode == 6 = runop (if val1 == 0 then val2 else (i+3)) (State mem input output)
  | opcode == 7 = runop (i+4) $ State { mem = M.insert dst (if val1 <  val2 then 1 else 0) mem,
                                        input = input, output = output }
  | opcode == 8 = runop (i+4) $ State { mem = M.insert dst (if val1 == val2 then 1 else 0) mem,
                                        input = input, output = output }
  | otherwise = error "invalid opcode"
    where opcode = mem ! i `mod` 100
          mode1  = mem ! i `mod` 1000  `div` 100
          mode2  = mem ! i `mod` 10000 `div` 1000
          src1   = mem ! (i+1)
          src2   = mem ! (i+2)
          dst    = mem ! (i+3)
          val1   = if mode1 == 1 && opcode `elem` [1,2,5,6,7,8] then src1 else (mem ! src1)
          val2   = if mode2 == 1 && opcode `elem` [1,2,5,6,7,8] then src2 else (mem ! src2)

runAmplifiers :: [Int] -> Int -> M.Map Int Int -> Int
runAmplifiers [] inSignal _ = inSignal
runAmplifiers (x:xs) inSignal mem = runAmplifiers xs outSignal mem
  where outSignal = head . output $ runop 0 (State mem [x,inSignal] [])

main = do
  contents <- getContents
  let input = M.fromList $ zip [0..] $ Prelude.map (read :: String -> Int) w
              where w = words [if x == ',' then ' ' else x | x <- contents]
  print . maximum $ Prelude.map (\i -> runAmplifiers i 0 input) $ permutations [0..4]
