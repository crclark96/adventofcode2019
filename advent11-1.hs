import Data.Map.Lazy as M

data State = State { mem :: M.Map Int Int,
                     ip :: Int,
                     halted :: Bool,
                     input :: [ Int ],
                     output :: [ Int ],
                     base :: Int } deriving Show
data Direction = Right | Left | Up | Down
type Robot = (Coords, Direction, State)
type Coords = (Int,Int)
type Grid = M.Map Coords Int

run :: State -> State
run s
  | halted s == True = s
  | opcode == 3 && input s == [] = s
  | otherwise = run $ runop s
    where opcode = (mem s) ! (ip s) `mod` 100

runop :: State -> State
runop (State mem ip halted input output base)
  | opcode == 99 = (State mem ip True input output base)
  | opcode == 1 = State { mem = M.insert dst (val1 + val2) mem,
                          input = input, output = output,
                          base = base, ip = ip+4, halted = False }
  | opcode == 2 = State { mem = M.insert dst (val1 * val2) mem,
                          input = input, output = output,
                          base = base, ip = ip+4, halted = False }
  | opcode == 3 = State { mem = M.insert src1 (head input) mem,
                          input = tail input, output = output,
                          base = base, ip = ip+2, halted = False }
  | opcode == 4 = State { mem = mem, input = input,
                          output = (val1:output), base = base,
                          ip = ip+2, halted = False }
  | opcode == 5 = State { mem = mem, input = input, output = output,
                          base = base, ip = if val1 /= 0 then val2 else ip+3,
                          halted = False }
  | opcode == 6 = State { mem = mem, input = input, output = output,
                          base = base, ip = if val1 == 0 then val2 else ip+3,
                          halted = False }
  | opcode == 7 = State { mem = M.insert dst (if val1 <  val2 then 1 else 0) mem,
                          input = input, output = output, base = base,
                          ip = ip+4, halted = False }
  | opcode == 8 = State { mem = M.insert dst (if val1 == val2 then 1 else 0) mem,
                          input = input, output = output, base = base,
                          ip = ip+4, halted = False }
  | opcode == 9 = State { mem = mem, input = input,
                          output = output, base = (base+val1),
                          ip = ip+2, halted = False }
  | otherwise = error "invalid opcode"
    where opcode = access ip mem `mod` 100
          mode1  = access ip mem `mod` 1000   `div` 100
          mode2  = access ip mem `mod` 10000  `div` 1000
          mode3  = access ip mem `mod` 100000 `div` 10000
          src1   = case mode1 of
                    0 -> access (ip+1) mem
                    1 -> access (ip+1) mem
                    2 -> access (ip+1) mem + base
          src2   = case mode2 of
                    0 -> access (ip+2) mem
                    1 -> access (ip+2) mem
                    2 -> access (ip+2) mem + base
          dst    = case mode3 of
                    0 -> access (ip+3) mem
                    1 -> access (ip+3) mem
                    2 -> access (ip+3) mem + base
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

go :: Grid -> Robot -> Grid
go m (c,d,s)
  |

main = do
  contents <- getContents
  let input = M.fromList $ zip [0..] $ Prelude.map (read :: String -> Int) $ words [if x == ',' then ' ' else x | x <- contents]
      m     = M.empty
      state = State { mem = input, ip = 0, halted = False,
                      output = [], input = [], base = 0 }
      r     = go m ((0,0),Up,state)
  print "help"
