import qualified Data.Map.Lazy as M

type Coords = (Int,Int)
data Grid = Grid { recent :: Coords, len :: Int, grid :: M.Map Coords Int } deriving Show

startGrid :: Grid
startGrid = Grid { recent = (0,0), grid = M.empty, len = 0 }

addLeg :: String -> Grid -> Grid
addLeg (c:xs) g
  | d == 0 = g
  | c == 'R' = addLeg (c:i) $ markGrid (x+1, y) g
  | c == 'L' = addLeg (c:i) $ markGrid (x-1, y) g
  | c == 'U' = addLeg (c:i) $ markGrid (x, y+1) g
  | c == 'D' = addLeg (c:i) $ markGrid (x, y-1) g
    where d = (read xs :: Int)
          x = fst $ recent g
          y = snd $ recent g
          i = show $ (d-1)

markGrid :: Coords -> Grid -> Grid
markGrid c (Grid r l g) = Grid { recent = c, grid = M.insert c (l+1) g, len = l+1 }

closestIntersectionDistance :: Grid -> Grid -> Int
closestIntersectionDistance (Grid r0 l0 g0) (Grid r1 l1 g1) = minimum . map snd . M.toList $ M.intersectionWith (+) g0 g1

buildGrid :: String -> Grid
buildGrid "" = startGrid
buildGrid s  = foldr addLeg startGrid $ reverse l
                where l = words [if x == ',' then ' ' else x | x <- s]

main = do
  input <- getContents
  let g0 = buildGrid $ lines input !! 0
      g1 = buildGrid $ lines input !! 1
  print $ closestIntersectionDistance g0 g1
