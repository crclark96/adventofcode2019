import qualified Data.Map.Lazy as M
import Data.Maybe (fromJust)

addObject :: String -> M.Map String String -> M.Map String String
addObject orbit m = M.insert satellite body m
  where body      = objects !! 0
        satellite = objects !! 1
        objects   = words [if x == ')' then ' ' else x | x <- orbit]

numOrbits :: String -> M.Map String String -> Int
numOrbits object m
  | body == Nothing = 0
  | otherwise = numOrbits' 1 (fromJust body) m
      where body = M.lookup object m

numOrbits' :: Int -> String -> M.Map String String -> Int
numOrbits' n object m
  | body == Nothing = n
  | otherwise = numOrbits' (n+1) (fromJust body) m
      where body = M.lookup object m

main = do
  input <- readFile "input/input06"
  let orbits = foldr addObject M.empty $ reverse $ lines input
  print $ sum . map (\k -> numOrbits k orbits) $ M.keys orbits


