module JumpgatesData 
  ( jg_startingJumpgates
  )
where

import Jumpgates
import Navigation
import Vector

jg_startingJumpgates = zip [0..]
                           [ jg_io
                           , jg_earth
                           ]

jg :: String -> Vector3D -> Jumpgate
jg n v = Jumpgate n v (toHyper v)

jg_io = jg "Io" (fromList [1,2,3])
jg_earth = jg "Earth" (fromList [5,6,7])
