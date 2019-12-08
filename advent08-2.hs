import Data.Char (isDigit)

width = 25
height = 6

layer :: Int -> [a] -> [[a]]
layer d [] = [[]]
layer d a = case splitAt d a of
  (x,xs) -> layer' d xs [x]

layer' :: Int -> [a] -> [[a]] -> [[a]]
layer' _ [] ls = ls
layer' d a ls = case splitAt d a of
  (x,xs) -> layer' d xs (x:ls)

drawPicture :: [[Int]] -> [Int]
drawPicture (x:xs) = drawPicture' xs x

drawPicture' :: [[Int]] -> [Int] -> [Int]
drawPicture' [] p = p
drawPicture' (l:layers) picture = drawPicture' layers newPicture
  where newPicture = zipWith resolvePixel l picture

resolvePixel :: Int -> Int -> Int
resolvePixel 2 y = y
resolvePixel x _ = x

makePretty :: [Int] -> String
makePretty = map (\x -> if x == 0 then ' ' else '=')

main = do
  input <- readFile "input/input08"
  let pixels = layer (height*width) $ map (\x -> read [x] :: Int) $ filter isDigit input
      picture = drawPicture pixels
  mapM_ putStrLn $ reverse $ map makePretty $ layer width picture

