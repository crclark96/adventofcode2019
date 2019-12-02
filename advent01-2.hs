fuel :: Int -> Int
fuel mass = if fuelmass <= 0
              then 0
              else fuelmass + (fuel fuelmass)
                where fuelmass = mass `div` 3 - 2


main = do
  contents <- getContents
  let masses = filter (\x -> length x > 0) $ lines contents
      fuels = map (fuel . read :: String -> Int) masses
      total = sum fuels
  print total
