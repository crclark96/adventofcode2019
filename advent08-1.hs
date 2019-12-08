import Data.Char (isDigit)

width = 25
height = 6

layer :: [a] -> [[a]]
layer [] = [[]]
layer a = case splitAt (height*width) a of
  (x,xs) -> layer' xs [x]

layer' :: [a] -> [[a]] -> [[a]]
layer' [] ls = ls
layer' a ls = case splitAt (height*width) a of
  (x,xs) -> layer' xs (x:ls)

numElem :: (Eq a) => a -> [a] -> Int
numElem x xs = foldr (\elem acc -> if elem == x then acc+1 else acc) 0 xs

hasFewest :: (Eq a) => a -> [[a]] -> [a]
hasFewest x xs = head $ filter (\subl -> numElem x subl <= fewest) xs
  where fewest = minimum $ map (numElem x) xs

main = do
  input <- readFile "input/input08"
  let pixels = map (\x -> read [x] :: Int) $ filter isDigit input
      l = hasFewest 0 $ layer pixels
  print $ (numElem 1 l) * (numElem 2 l)

