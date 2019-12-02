fuel :: Int -> Int
fuel mass
  | fuelmass <= 0 = 0
  | otherwise = fuelmass + (fuel fuelmass)
      where fuelmass = mass `div` 3 - 2


main = do
  contents <- getContents
  print $ sum [fuel $ read x :: Int | x <- lines contents, length x > 0]
