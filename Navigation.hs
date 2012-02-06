module Navigation where

import Prelude hiding (length)

import GlobalConst (tickGame)
import Vector
import Wrappers

data NavModule = NavModule 
    { nav_position :: NavPosition 
    , nav_status :: NavStatus }
    deriving ()

data NavPosition = DockedToStation StationID
                 | DockedToShip ShipID
                 | Space Vector3D SpaceType
              -- | Thirdspace { x :: Int, y :: Int, z :: Int }
    deriving (Eq, Show)

data NavStatus = Idle 
               | DockingToStation StationID 
               | DockingToShip ShipID
               | MovingTo { navMoving_velocity :: Vector3D
                          , navMoving_target :: Vector3D }
    deriving (Show)

data SpaceType = Normalspace
               | Hyperspace
            -- | Thirdspace
    deriving (Eq, Show)

instance Show NavModule where
    show (NavModule p s) = "Showing navigation module: \n" ++ show p ++ "\n and:\n" ++ show s

tick = fromInteger tickGame -- WARNING: MAGIC CONSTANT
                            -- tickGame imported from GlobalConst

defaultNavModule = NavModule (Space (fromList [0,0,0]) Normalspace)  Idle

dockingStNS :: NavStatus -> Bool
dockingStNS (DockingToStation _) = True
dockingStNS _ = False

dockingStNSID :: NavStatus -> StationID
dockingStNSID (DockingToStation i) = i
dockingStNSID _ = undefined

updateNavStatus :: NavModule -> NavModule -- ignores SpaceType FIXME
updateNavStatus m@(NavModule pos Idle) = m 
updateNavStatus m@(NavModule (Space pos st) (MovingTo vel targ)) =
    let posIfKeepMoving = pos + vel * tick
        closeEnough = length( targ-pos ) <= length( vel*tick )
    in if closeEnough then NavModule (Space targ st) Idle
                      else NavModule (Space posIfKeepMoving st) (MovingTo vel targ)
