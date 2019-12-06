import qualified Data.Map.Lazy as M
import qualified Data.Set as S
import Data.Maybe (fromJust)

addObject :: String -> M.Map String String -> M.Map String String
addObject orbit m = M.insert satellite body m
  where body      = objects !! 0
        satellite = objects !! 1
        objects   = words [if x == ')' then ' ' else x | x <- orbit]

getOrbits :: (Ord a) => a -> M.Map a a -> S.Set a
getOrbits obj m = case M.lookup obj m of
  Nothing -> S.empty
  Just body -> S.insert body $ getOrbits body m

main = do
  input <- readFile "input/input06"
  let orbits    = foldr addObject M.empty $ reverse $ lines input
      meOrbits  = getOrbits "YOU" orbits
      sanOrbits = getOrbits "SAN" orbits
      intersection = S.intersection meOrbits sanOrbits
      union     = S.union meOrbits sanOrbits
  print $ (S.size union) - (S.size intersection)
