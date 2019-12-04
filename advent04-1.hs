match :: Int -> Bool
match i = len && inc && adj
  where len   = length str == 6
        inc   = foldl (\acc x -> fst x <= snd x && acc) True  pairs
        adj   = foldl (\acc x -> fst x == snd x || acc) False pairs
        str   = show i
        pairs = zip str $ tail str

main = do
  print . length $ filter match [272091..815432]
