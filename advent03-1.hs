import qualified Data.Set as S

type Coords = (Int,Int)
data Grid = Grid { recent :: Coords, grid :: S.Set Coords } deriving Show

startGrid :: Grid
startGrid = Grid { recent = (0,0), grid = S.empty }

addLeg :: String -> Grid -> Grid
addLeg (c:xs) g
  | d == 0 = g
  | c == 'R' = addLeg (c:i) $ markGrid (x+1,y) g
  | c == 'L' = addLeg (c:i) $ markGrid (x-1,y) g
  | c == 'U' = addLeg (c:i) $ markGrid (x,y+1) g
  | c == 'D' = addLeg (c:i) $ markGrid (x,y-1) g
    where d = (read xs :: Int)
          x = fst $ recent g
          y = snd $ recent g
          i = show $ (d-1)

markGrid :: Coords -> Grid -> Grid
markGrid c g = Grid { recent = c, grid = S.insert c $ grid g }

dist :: (Int,Int) -> Int
dist (x,y) = (abs x) + (abs y)

closestIntersectionDistance :: Grid -> Grid -> Int
closestIntersectionDistance (Grid r0 g0) (Grid r1 g1) = minimum . map dist . S.toList $ S.intersection g0 g1

buildGrid :: String -> Grid
buildGrid "" = startGrid
buildGrid s  = foldr addLeg startGrid $ reverse l
                where l = words [if x == ',' then ' ' else x | x <- s]

main = do
  input <- getContents
  let g0 = buildGrid $ lines input !! 0
      g1 = buildGrid $ lines input !! 1
  print $ closestIntersectionDistance g0 g1
