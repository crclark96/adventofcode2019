import qualified Data.Set as S

type Coords = (Int, Int)
type Grid = S.Set Coords

createGrid :: [String] -> Grid
createGrid m = createGrid' 0 0 m S.empty

createGrid' :: Int -> Int -> [String] -> Grid -> Grid
createGrid' x y m g
  | x >= w    = createGrid' 0 (y+1) m g
  | y >= l    = g
  | otherwise = createGrid' (x+1) y m g'
    where g' = if m !! y !! x == '#' then S.insert (x,y) g else g
          l  = length m
          w  = length (m !! 0)

angle :: Coords -> Coords -> Float
angle (x1,y1) (x2,y2) = atan (y/x) + q
  where x = fromIntegral $ x2 - x1
        y = fromIntegral $ y2 - y1
        q = case (x,y) of _
                           | (x <= 0 && y >= 0) ->  pi
                           | (x <= 0 && y <= 0) -> -pi
                           | otherwise          ->   0

numVisible :: Coords -> Grid -> Int
numVisible c g = S.size . S.map (\x -> angle c x) $ S.delete c g

visible :: Coords -> Grid -> S.Set (Coords, Float)
visible c g = S.map (\x -> (x, angle c x)) $ S.delete c g

main = do
  input <- readFile "input/input10"
  let m = lines input
      g = createGrid m
      station = fst $ foldr1 (\x acc -> if snd x > snd acc then x else acc) $ S.map (\x -> (x, numVisible x g)) g
      angles  = S.map (\x -> (x, angle station x)) $ S.delete station g
      q1 = S.filter (\(c,a) -> 0<a && a<(pi/2)) angles
      q2 = S.filter (\(c,a) -> (-pi/2)<a && a<0) angles
      q3 = S.filter (\(c,a) -> (-pi)<a && a<(-pi/2)) angles
      q4 = S.filter (\(c,a) -> (pi/2)<a && a<pi) angles
      n  = 200 - S.size q1 - S.size q2 - 1
  print $ (S.toDescList $ S.map (\(c,a) -> (a,c)) q3) !! n

