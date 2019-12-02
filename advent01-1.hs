fuel :: Int -> Int
fuel mass = mass `div` 3 - 2

main = do
  contents <- getContents
  print $ sum [fuel $ read x :: Int | x <- lines contents, length x > 0]
