module Data.Jumpgates 
  ( jg_startingJumpgates
  )
where

import qualified Data.Map as M

import Data.Maybe (isJust, fromJust)
import Jumpgates
import Navigation
import Space
import Vector

jg_startingJumpgates = [ jg_io
                       , jg_earth
                       ]

-- To add the possibility of ingame jumpgate creation, one would need
-- to move this map into a TVar. TODO sometime later
jgsAdjacency :: JumpgateNetwork
jgsAdjacency = M.fromList
  [ ((jg_earth, jg_io)                     ,  900.0)
  , ((jg_alphaCentauri, jg_earth)          ,  700.0)
  , ((jg_alphaCentauri, jg_proximaCentauri), 1500.0)
  ]

jgsDistance :: Jumpgate -> Jumpgate -> Double
jgsDistance jg1 jg2 =
  let p1 = (jg1, jg2)
      p2 = (jg2, jg1)
      m1 = M.lookup p1 jgsAdjacency
      m2 = M.lookup p2 jgsAdjacency
  in case (isJust m1, isJust m2) of
       (True, _) -> fromJust m1
       (_, True) -> fromJust m2
       otherwise -> error "Jumpgates: jgsDistance: Jumpgates not adjacent"

jgsAdjacentP :: Jumpgate -> Jumpgate -> Bool
jgsAdjacentP jg1 jg2 = not . null $
  filter (\p -> p == (jg1, jg2) || p == (jg2, jg1)) (M.keys jgsAdjacency)

jg_adjacent :: Jumpgate -> [Jumpgate]
jg_adjacent jg =
  let fn :: [Jumpgate] -> (Jumpgate, Jumpgate) -> [Jumpgate]
      fn acc (a, b) | jg == a = acc ++ [b]
                    | jg == b = acc ++ [a]
                    | otherwise = acc
  in foldl fn [] (M.keys jgsAdjacency)

jg :: String -> Vector3D -> Jumpgate
jg n v = 
  let (Space v1 _) = toHyper (Space v Normalspace)
  in  Jumpgate n v v1

jg_io = jg "Io" (fromList [1,2,3])
jg_earth = jg "Earth" (fromList [5,6,7])
jg_alphaCentauri = jg "Alpha Centauri" (fromList [11.2, 13.1, 198.17])
jg_proximaCentauri = jg "Proxima Centauri" (fromList [11.9, 90.7, 165.14])
