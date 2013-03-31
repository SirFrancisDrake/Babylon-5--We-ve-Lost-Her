module JumpgatesData 
  ( jg_startingJumpgates
  )
where

import Data.Maybe (isJust)
import Jumpgates
import Navigation
import Space
import Vector

jg_startingJumpgates = zip [0..]
                           [ jg_io
                           , jg_earth
                           ]

jg :: String -> Vector3D -> Jumpgate
jg n v = 
  let (Space v1 _) = toHyper (Space v Normalspace)
  in  Jumpgate n v v1

jg_io = jg "Io" (fromList [1,2,3])
jg_earth = jg "Earth" (fromList [5,6,7])
jg_alphaCentauri = jg "Alpha Centauri" (fromList [11.2, 13.1, 198.17])
jg_proximaCentauri = jg "Proxima Centauri" (fromList [11.9, 90.7, 165.14])

jg_adjacencyTable :: [(Jumpgate, Jumpgate)]
jg_adjacencyTable =
  [ (jg_io, jg_earth)
  , (jg_earth, jg_alphaCentauri)
  , (jg_alphaCentauri, jg_proximaCentauri)
  ]

jg_adjacent :: Jumpgate -> [Jumpgate]
jg_adjacent jg =
  let fn :: [Jumpgate] -> (Jumpgate, Jumpgate) -> [Jumpgate]
      fn acc (a, b) | jg == a = acc ++ [b]
                    | jg == b = acc ++ [a]
                    | otherwise = acc
  in foldl fn [] jg_adjacencyTable
