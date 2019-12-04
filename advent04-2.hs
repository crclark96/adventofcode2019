import Data.List (group)

match :: Int -> Bool
match i = len && inc && adj
  where len   = length str == 6
        inc   = foldl (\acc x -> fst x <= snd x && acc) True  pairs
        adj   = elem 2 . map length $ group str
        str   = show i
        pairs = zip str $ tail str

main = do
  print $ length . filter (\x -> x) $ map match [272091..815432]
