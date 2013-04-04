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

defaultNavModule = NavModule (SNPSpace $ Space (V.fromList [0,0,0]) Normalspace) Idle []

defaultShipClass = Rhino

shipStats _ = ShipStats 1 3 10 100 200

firstEmptyKey :: IntMap a -> Int
firstEmptyKey = fst . findMax

defaultShipStats :: ShipClass -> ShipStats
defaultShipStats _ = ShipStats 1 3 10 100 200

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

isMoving :: Ship -> Bool
isMoving s =
  case navModule_status (ship_navModule s) of
    MovingToSpace _ _ -> True
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

ship_freeSpace :: Ship -> Weight
ship_freeSpace s = fromIntegral( shipStats_cargoHold $ ship_stats s ) - weight (ship_cargo s)

tick = tickGame -- WARNING: MAGIC CONSTANT
                -- tickGame imported from GlobalConst

tickMove :: NavModule -> NavModule -- ignores SpaceType FIXME
tickMove m@(NavModule pos Idle _) = m 
tickMove m@(NavModule (SNPSpace (Space pos st)) (MovingToSpace vel targ) p) =
    let diff = vel * (fromInteger tickGame)
        posIfKeepMoving = pos + diff
        closeEnough = length( targ-pos ) <= length diff
    in if closeEnough then NavModule (SNPSpace (Space targ st)) Idle p
                      else NavModule (SNPSpace (Space posIfKeepMoving st)) (MovingToSpace vel targ) p
tickMove m@(NavModule (InHyperspaceBetween p1@(jg1, d1) p2@(jg2, d2)) s@(MovingInHyperspace vel targ) p) =
    let targDist = if targ == jg1 then d1 else d2
        distCovered = vel * (fromIntegral tick)
        pr (jg,d) = if targ == jg then (jg, d - distCovered) else (jg, d + distCovered)
    in  if distCovered >= targDist
          then NavModule (OnJumpgate targ) Idle p
          else NavModule (InHyperspaceBetween (pr p1) (pr p2)) s p
tickMove n = n
