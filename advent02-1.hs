import Data.Map.Lazy as M

runop :: Int -> M.Map Int Int -> M.Map Int Int
runop i list
  | opcode == 99 = list
  | opcode == 1 = runop (i+4) $ M.insert dst (src1 + src2) list
  | opcode == 2 = runop (i+4) $ M.insert dst (src1 * src2) list
    where opcode = list ! i
          src1   = list ! (list ! (i+1))
          src2   = list ! (list ! (i+2))
          dst    = list ! (i+3)

main = do
  contents <- getContents
  let input = M.fromList $ zip [0..] $ Prelude.map (read :: String -> Int) $ words [if x == ',' then ' ' else x | x <- contents]
      state = M.insert 1 12 $ M.insert 2 2 input
  print $ (runop 0 state) ! 0
