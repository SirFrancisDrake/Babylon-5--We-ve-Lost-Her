module Navigation where

import ErrorMessages
import Vector
import Wrappers

data NavPosition = Space { nav_pos_vec :: Vector3D
                         , nav_pos_type :: SpaceType }
    deriving (Eq, Show)

data SpaceType = Normalspace
               | Hyperspace
            -- | Thirdspace
    deriving (Eq, Show)

spaceDistanceDumb :: NavPosition -> NavPosition -> Double
spaceDistanceDumb a@(Space v1 t1) b@(Space v2 t2)
    | t1 == t2 = distance v1 v2
    | otherwise = err_distanceNormalHyper
