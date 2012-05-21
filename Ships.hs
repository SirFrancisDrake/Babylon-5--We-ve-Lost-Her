module Ships where

import Control.Concurrent.STM
import Data.IntMap hiding (fromList)
import qualified Data.IntMap as I (fromList)
import Prelude hiding (length)

import Currency
import DataTypes
import ErrorMessages
import GlobalConst (tickGame)
import Navigation
import PersonalData
import ShipStats
import Stock
import Vector hiding (fromList)
import qualified Vector as V (fromList)
import Wares
import Wrappers

-- defaultShips = I.fromList $ zip [1..] 
--                                 [ rimbauld
--                                 , goldenHind
--                                 ]
-- 
-- rimbauld :: Ship
-- rimbauld = defaultShip{ ship_name = "Rimbauld" } -- 1 for Helen Ripley CARE
-- 
-- goldenHind :: Ship
-- goldenHind = defaultShip { ship_name = "Golden Hind" } -- 1 for Helen Ripley CARE
-- 
-- makeDefaultShip :: STM (TVar Owner, TVar Ship)
-- makeDefaultShip = do
--     emptyOwner <- newTVar $ Owner "" [] [] (Person Human Merchant) 0
--     defaultShip <- newTVar $ Ship   "empty name"
--                                     defaultShipClass 
--                                     (shipStats defaultShipClass) 
--                                     defaultNavModule 
--                                     SAINone
--                                     defaultCargo 
--                                     emptyOwner
--     (emptyOwner, defaultShip)

defaultNavModule = NavModule (SNPSpace $ Space (V.fromList [0,0,0]) Normalspace) Idle

defaultShipClass = Rhino

shipStats _ = ShipStats 1 10 100

firstEmptyKey :: IntMap a -> Int
firstEmptyKey = fst . findMax

defaultShipStats :: ShipClass -> ShipStats
defaultShipStats _ = ShipStats 1 10 100

undocking :: Ship -> Bool
undocking s = undockingStNS (navModule_status $ ship_navModule s)
              where undockingStNS Undocking = True
                    undockingStNS _ = False

docking :: Ship -> Bool
docking s = dockingStNS (navModule_status $ ship_navModule s)
            where dockingStNS (DockingToStation _) = True
                  dockingStNS _ = False

dockingSt :: Ship -> (TVar Station)
dockingSt = dockingStNS . navModule_status . ship_navModule
            where dockingStNS (DockingToStation tst) = tst
                  dockingStNS _ = err_dockingStNotExist

docked :: Ship -> Bool
docked s = dockedStNS (navModule_position $ ship_navModule s)
           where dockedStNS (DockedToStation _) = True
                 dockedStNS _ = False

dockedSt :: Ship -> (TVar Station)
dockedSt = dockedStNS . navModule_position . ship_navModule
           where dockedStNS (DockedToStation tst) = tst
                 dockedStNS _ = err_dockedStNotExist

dockedToSt :: Ship -> (TVar Station) -> Bool
dockedToSt = dockedToStNS . navModule_position . ship_navModule
             where dockedToStNS (DockedToStation i) j = i == j
                   dockedToStNS _ _ = False

setShipOnCourse :: Ship -> (TVar Station) -> Ship
setShipOnCourse s tst = let (NavModule pos _) = ship_navModule s
                        in s{ ship_navModule = NavModule pos (MovingToStation tst) }

ship_freeSpace :: Ship -> Weight
ship_freeSpace s = fromIntegral( shipStats_cargoHold $ ship_stats s ) - weight (ship_cargo s)

tick = fromInteger tickGame -- WARNING: MAGIC CONSTANT
                            -- tickGame imported from GlobalConst

updateNavStatus :: NavModule -> NavModule -- ignores SpaceType FIXME
updateNavStatus m@(NavModule pos Idle) = m 
updateNavStatus m@(NavModule (SNPSpace (Space pos st)) (MovingToSpace vel targ)) =
    let posIfKeepMoving = pos + vel * tick
        closeEnough = length( targ-pos ) <= length( vel*tick )
    in if closeEnough then NavModule (SNPSpace (Space targ st)) Idle
                      else NavModule (SNPSpace (Space posIfKeepMoving st)) (MovingToSpace vel targ)
