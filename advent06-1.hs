import qualified Data.Map.Lazy as M
import Data.Maybe (fromJust)

addObject :: String -> M.Map String String -> M.Map String String
addObject orbit m = M.insert satellite body m
  where body      = objects !! 0
        satellite = objects !! 1
        objects   = words [if x == ')' then ' ' else x | x <- orbit]

numOrbits :: String -> M.Map String String -> Int
numOrbits object m = case M.lookup object m of
  Nothing -> 0
  Just x  -> numOrbits' 1 x m

numOrbits' :: Int -> String -> M.Map String String -> Int
numOrbits' n object m = case M.lookup object m of
  Nothing -> n
  Just x  -> numOrbits' (n+1) x m

main = do
  input <- readFile "input/input06"
  let orbits = foldr addObject M.empty $ reverse $ lines input
  print $ sum . map (\k -> numOrbits k orbits) $ M.keys orbits


