type Velocity = (Int,Int,Int)
type Position = Velocity
data Planet = Planet { pos :: Position, vel :: Velocity } deriving (Show, Eq, Ord)

updatePosition :: Planet -> Planet
updatePosition (Planet (px,py,pz) (vx,vy,vz)) = Planet (px+vx,py+vy,pz+vz) (vx,vy,vz)

applyGravity :: Planet -> Planet -> Planet
-- applies gravity from planet 1 to planet 2
applyGravity (Planet (x1,y1,z1) v1) (Planet (x2,y2,z2) (vx2,vy2,vz2)) = p2'
  where p2' = Planet (x2,y2,z2) (c x1 x2 + vx2, c y1 y2 + vy2, c z1 z2 + vz2)
        c u v = fromEnum (u `compare` v) - 1

applyGravityAll :: [Planet] -> [Planet]
applyGravityAll (x:[]) = [x]
applyGravityAll (x:xs) = foldr applyGravity x xs : (applyGravityAll $ map (applyGravity x) xs)

potentialEnergy :: Planet -> Int
potentialEnergy (Planet (x,y,z) _) = abs x + abs y + abs z

kineticEnergy :: Planet -> Int
kineticEnergy (Planet _ (vx,vy,vz)) = abs vx + abs vy + abs vz

totalSystemEnergy :: [Planet] -> Int
totalSystemEnergy xs = sum $ map (\x -> kineticEnergy x * potentialEnergy x) xs

runStep :: Int -> [Planet] -> [Planet] -> Int
runStep i s xs
  | s == xs && i > 0 = i
  | otherwise = runStep (i+1) s $ map updatePosition $ applyGravityAll xs

main = do
  let lx = [Planet (3,0,0) (0,0,0), Planet (4,0,0) (0,0,0),
            Planet (-10,0,0) (0,0,0), Planet (-3,0,0) (0,0,0)]
  let ly = [Planet (0,3,0) (0,0,0), Planet (0,-16,0) (0,0,0),
            Planet (0,-6,0) (0,0,0), Planet (0,0,0) (0,0,0)]
  let lz = [Planet (0,0,0) (0,0,0), Planet (0,0,2) (0,0,0),
            Planet (0,0,5) (0,0,0), Planet (0,0,-13) (0,0,0)]
  print $ lcm (runStep 0 lx lx) $ lcm (runStep 0 ly ly) (runStep 0 lz lz)
-- input:
-- <x=3, y=3, z=0>
-- <x=4, y=-16, z=2>
-- <x=-10, y=-6, z=5>
-- <x=-3, y=0, z=-13>



