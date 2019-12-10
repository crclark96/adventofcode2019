import qualified Data.Set as S
import qualified Data.Map.Lazy as M

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
angle (x1,y1) (x2,y2) = if a < 0 then a + 2*pi else a
  where x = fromIntegral $ x2 - x1
        y = fromIntegral $ y1 - y2
        a = atan2 y x

numVisible :: Coords -> Grid -> Int
numVisible c g = S.size . S.map (\x -> angle c x) $ S.delete c g

visible :: Coords -> Grid -> Grid
visible c g = S.fromList $ M.elems m
  where m  = M.fromListWith (\c1 c2 -> if dist c1 c < dist c2 c then c1 else c2) $ S.elems $ S.map (\x -> (angle c x,x)) $ S.delete c g

dist :: Coords -> Coords -> Float
dist (x1,y1) (x2,y2) = abs $ sqrt (x^2 + y^2)
  where x = fromIntegral $ x2 - x1
        y = fromIntegral $ y2 - y1

main = do
  input <- readFile "input/input10"
  let m = lines input
      g = createGrid m
      station = fst $ foldr1 (\x acc -> if snd x > snd acc then x else acc) $ S.map (\x -> (x, numVisible x g)) g
      v = visible station g
      angles  = S.map (\x -> (angle station x,x)) v
      q1 = S.toDescList $ S.filter (\(a,c) -> 0<a && a<=(pi/2)) angles
      q2 = S.toDescList $ S.filter (\(a,c) -> (pi/2)<a && a<=pi) angles
      q3 = S.toDescList $ S.filter (\(a,c) -> pi<a && a<=(3/2)*pi) angles
      q4 = S.toDescList $ S.filter (\(a,c) -> (3/2)*pi<a || a==0) angles
  print $ (q1 ++ q4 ++ q3 ++ q2) !! 199
