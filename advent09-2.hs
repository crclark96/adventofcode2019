import Data.Map.Lazy as M

data State = State { mem :: M.Map Int Int,
                     input :: [ Int ],
                     output :: [ Int ],
                     base :: Int } deriving Show

runop :: Int -> State -> State
runop i (State mem input output base)
  | opcode == 99 = (State mem input output base)
  | opcode == 1 = runop (i+4) $ State { mem = M.insert dst (val1 + val2) mem,
                                        input = input, output = output,
                                        base = base }
  | opcode == 2 = runop (i+4) $ State { mem = M.insert dst (val1 * val2) mem,
                                        input = input, output = output,
                                        base = base }
  | opcode == 3 = runop (i+2) $ State { mem = M.insert src1 (head input) mem,
                                        input = tail input, output = output,
                                        base = base }
  | opcode == 4 = runop (i+2) $ State { mem = mem, input = input,
                                        output = (val1:output), base = base }
  | opcode == 5 = runop (if val1 /= 0 then val2 else (i+3)) (State mem input output base)
  | opcode == 6 = runop (if val1 == 0 then val2 else (i+3)) (State mem input output base)
  | opcode == 7 = runop (i+4) $ State { mem = M.insert dst (if val1 <  val2 then 1 else 0) mem,
                                        input = input, output = output, base = base }
  | opcode == 8 = runop (i+4) $ State { mem = M.insert dst (if val1 == val2 then 1 else 0) mem,
                                        input = input, output = output, base = base }
  | opcode == 9 = runop (i+2) $ State { mem = mem, input = input,
                                        output = output, base = (base+val1) }
  | otherwise = error "invalid opcode"
    where opcode = access i mem `mod` 100
          mode1  = access i mem `mod` 1000   `div` 100
          mode2  = access i mem `mod` 10000  `div` 1000
          mode3  = access i mem `mod` 100000 `div` 10000
          src1   = case mode1 of
                    0 -> access (i+1) mem
                    1 -> access (i+1) mem
                    2 -> access (i+1) mem + base
          src2   = case mode2 of
                    0 -> access (i+2) mem
                    1 -> access (i+2) mem
                    2 -> access (i+2) mem + base
          dst    = case mode3 of
                    0 -> access (i+3) mem
                    1 -> access (i+3) mem
                    2 -> access (i+3) mem + base
          val1   = case mode1 of
                    0 -> access src1 mem
                    1 -> src1
                    2 -> access src1 mem
          val2   = case mode2 of
                    0 -> access src2 mem
                    1 -> src2
                    2 -> access src2 mem

access :: Int -> M.Map Int Int -> Int
access i m = case M.lookup i m of
  Nothing -> 0
  Just v  -> v

main = do
  contents <- getContents
  let input = M.fromList $ zip [0..] $ Prelude.map (read :: String -> Int) $ words [if x == ',' then ' ' else x | x <- contents]
      prog  = M.fromList $ zip [0..] [3,1,4,1,109,1,203,0,4,2,99]
      state = State { mem = input, output = [], input = [2], base = 0 }
  print $ runop 0 state
