import Data.Map.Lazy as M

data State = State { mem :: M.Map Int Int, input :: [ Int ], output :: [ Int ] } deriving Show

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
  | otherwise = error "invalid opcode"
    where opcode = mem ! i `mod` 100
          mode1  = mem ! i `mod` 1000  `div` 100
          mode2  = mem ! i `mod` 10000 `div` 1000
          src1   = mem ! (i+1)
          src2   = mem ! (i+2)
          dst    = mem ! (i+3)
          val1   = if mode1 == 1 && opcode `elem` [1,2] then src1 else (mem ! src1)
          val2   = if mode2 == 1 && opcode `elem` [1,2] then src2 else (mem ! src2)

main = do
  contents <- getContents
  let input = M.fromList $ zip [0..] $ Prelude.map (read :: String -> Int) $ words [if x == ',' then ' ' else x | x <- contents]
      state = State { mem = input, output = [], input = [1] }
  print $ output (runop 0 state)
