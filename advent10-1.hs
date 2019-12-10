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
  print . S.findMax $ S.map (\x -> numVisible x g) g

