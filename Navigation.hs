module Navigation where

import Vector
import Wrappers

data NavPosition = Space { nav_pos_vec :: Vector3D
                         , nav_pos_type :: SpaceType }
    deriving (Eq, Show)

data SpaceType = Normalspace
               | Hyperspace
            -- | Thirdspace
    deriving (Eq, Show)
