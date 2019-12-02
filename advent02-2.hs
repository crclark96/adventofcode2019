search :: Int -> Int -> [Int] -> Int
search noun verb state
  | noun < 0 = search 99 (verb-1) state
  | result == 19690720 = 100 * noun + verb
  | otherwise = search (noun-1) verb state
    where result = (runop 0 $ replace 1 noun $ replace 2 verb state) !! 0

replace :: Int -> a -> [a] -> [a]
replace _ _ [] = []
replace i n (x:xs)
  | i == 0 = n:xs
  | otherwise = x:replace (i-1) n xs

runop :: Int -> [Int] -> [Int]
runop i list
  | opcode == 99 = list
  | opcode == 1 = runop (i+4) $ replace dst (src1 + src2) list
  | opcode == 2 = runop (i+4) $ replace dst (src1 * src2) list
    where opcode = list !! i
          src1   = list !! (list !! (i+1))
          src2   = list !! (list !! (i+2))
          dst    = list !! (i+3)

main = do
  contents <- getContents
  let input = map (read :: String -> Int) $ words [if x == ',' then ' ' else x | x <- contents]
      state = replace 1 12 $ replace 2 2 input
  print $ search 99 99 state
