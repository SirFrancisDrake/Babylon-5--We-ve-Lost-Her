module Navigation where

import Wrappers

data NavModule = NavModule NavPosition NavStatus
    deriving (Show)

data NavPosition = DockedToStation StationID
                 | DockedToShip ShipID
                 | Hyperspace { x :: Int, y :: Int, z :: Int }
                 | Normalspace { x :: Int, y :: Int, z :: Int }
              -- | Thirdspace { x :: Int, y :: Int, z :: Int }
    deriving (Eq, Show)

data NavStatus = Drifting
    deriving (Show)

defaultNavModule = NavModule (Hyperspace 0 0 0) Drifting

-- Desired way: NavStatus = Drifting (normalspace | hyperspace) x y z
--                        | Travelling
