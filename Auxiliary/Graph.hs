
module Auxiliary.Graph (Graph (..), findRoutes)
where

instance Graph Int where
  adjacentNodes 1 = [2]
  adjacentNodes 2 = [1,3,4]
  adjacentNodes 3 = [2]
  adjacentNodes 4 = [2,5,6]
  adjacentNodes 5 = [4]
  adjacentNodes 6 = [4]

class (Eq a) => Graph a where
  adjacentNodes :: a -> [a]
  adjacent :: a -> a -> Bool
  adjacent a b = 
    let adj x y = x `elem` (adjacentNodes y)
    in (a `adj` b) && (b `adj` a)

data Three =
  InProgress
  | Stuck
  | Success
  deriving (Eq)

spawn :: (Graph a) => a -> [a] -> [ ([a], Three) ]
spawn res route =
  let viableVs = filter (not . (flip elem route)) (adjacentNodes $ last route)
  in  if res `elem` (adjacentNodes $ last route)
        then [(route ++ [res], Success)]
        else if null viableVs
               then [(route, Stuck)]
               else map (\v -> (route ++ [v], InProgress)) viableVs

step :: (Graph a) => a -> [ ([a], Three) ] -> [ ([a], Three) ]
step end rs =
  let frd = filter ((/= Stuck) . snd) rs
      prc p@(r,t) 
        | t == Success = return p
        | otherwise = spawn end r
  in concatMap prc frd

process :: (Graph a) => a -> [ ([a], Three) ] -> [ ([a], Three) ]
process end rs =
  let unfinished cmp = not $ null $ filter ((== InProgress) . snd) cmp
      r = step end rs
  in if unfinished r
       then process end r
       else r

findRoutes :: (Graph a) => a -> a -> [[a]]
findRoutes start end =
  let first = spawn end [start]
  in foldl (\acc (r,t) -> if t == Success then acc ++ [r] else acc) [] (process end first)
