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

isIdle :: Ship -> Bool
isIdle s =
  case navModule_status (ship_navModule s) of
    Idle -> True
    otherwise -> False

jumping :: Ship -> Bool
jumping s = jumpingNS (navModule_status $ ship_navModule s)
            where jumpingNS (Jumping _ _) = True

dockingSt :: Ship -> (TVar Station)
dockingSt = dockingStNS . navModule_status . ship_navModule
            where dockingStNS (DockingToStation tst) = tst
                  dockingStNS _ = err_dockingStNotExist

dockedM :: Ship -> Maybe (TVar Station)
dockedM sh =
  if dockedP sh 
    then Just $ dockedSt sh
    else Nothing

dockedP :: Ship -> Bool
dockedP s = dockedStNS (navModule_position $ ship_navModule s)
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

setNavStatus :: (TVar Ship) -> NavStatus -> STM ()
setNavStatus tsh ns = readTVar tsh >>= \sh ->
  writeTVar tsh sh{ ship_navModule =
    (ship_navModule sh){ navModule_status = ns } }

setShipOnCourse :: (TVar Ship) -> (TVar Station) -> STM ()
setShipOnCourse tsh tst = setNavStatus tsh (MovingToStation tst)

startDockingTo :: (TVar Ship) -> (TVar Station) -> STM ()
startDockingTo tsh tst = setNavStatus tsh (DockingToStation tst)

startUndocking :: (TVar Ship) -> STM ()
startUndocking tsh = setNavStatus tsh Undocking

ship_freeSpace :: Ship -> Weight
ship_freeSpace s = fromIntegral( shipStats_cargoHold $ ship_stats s ) - weight (ship_cargo s)

tick = fromInteger tickGame -- WARNING: MAGIC CONSTANT
                            -- tickGame imported from GlobalConst

tickMove :: NavModule -> NavModule -- ignores SpaceType FIXME
tickMove m@(NavModule pos Idle _) = m 
tickMove m@(NavModule (SNPSpace (Space pos st)) (MovingToSpace vel targ) p) =
    let posIfKeepMoving = pos + vel * tick
        closeEnough = length( targ-pos ) <= length( vel*tick )
    in if closeEnough then NavModule (SNPSpace (Space targ st)) Idle p
                      else NavModule (SNPSpace (Space posIfKeepMoving st)) (MovingToSpace vel targ) p
