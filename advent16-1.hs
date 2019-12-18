import Data.Char (digitToInt, isDigit)
replace :: Int -> a -> [a] -> [a]
replace 0 a (x:xs) = a:xs
replace n a (x:xs) = x:(replace (n-1) a xs)

fft' :: Int -> [Int] -> Int
fft' n xs = calc xs pattern
  where pattern = tail . cycle $ dupl (n+1) [0,1,0,-1]
        dupl n xs = foldr (\x acc -> replicate n x ++ acc) [] xs
        calc a b = digitToInt . last . show . sum $ zipWith (*) a b

fft :: [Int] -> [Int]
fft xs = map (\(n,x) -> fft' n xs) $ zip [0..] xs

main = do
  input <- readFile "input/input16"
  let i = [digitToInt x | x <- input, isDigit x]
  print $ take 8 $ (iterate fft i !! 100)
