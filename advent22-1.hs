import Data.List

m :: Int
m = 10007

deal :: Int -> Int
deal n = (m-n-1) `mod` m

cut :: Int -> Int -> Int
cut i n = (n-i) `mod` m

incN :: Int -> Int -> Int
incN i n = n * i `mod` m

showCards :: [Int] -> [Int]
showCards xs = map fst h'
  where h  = zip [0..] xs
        h' = sortOn snd h

-- note: I edited the input to make pattern matching a bit cleaner
dispatch :: String -> Int -> Int
dispatch s = case words s of
              ["cut",n] -> cut $ read n
              ["deal"] -> deal
              ["increment",n] -> incN $ read n
              _ -> id

main = do
  input <- readFile "input/input22"
  let p = foldl (\acc x -> dispatch x acc) 2019 (lines input)
  print p
