module Ships where

import Control.Concurrent
import Control.Concurrent.STM
import Data.IntMap hiding (fromList)
import qualified Data.IntMap as I (fromList)
import Prelude hiding (length)

import AI
import IntMapAux
import GlobalConst (tickGame)
import Navigation
import ShipStats
import Vector hiding (fromList)
import qualified Vector as V (fromList)
import Wares
import Wrappers

type Ships = IntMap (TVar Ship)

modifyShip :: Int -> Ships -> (Ship -> Ship) -> IO ()
modifyShip i ships fn = atomically $ do
    let shipVar = ships ! i
    ship <- readTVar shipVar
    writeTVar shipVar (fn ship) 

data NavModule = NavModule { navModule_position :: ShipNavPosition
                           , navModule_status :: NavStatus
                           }
    deriving (Show)

data ShipNavPosition = DockedToStation StationID
                     | DockedToShip ShipID
                     | SNPSpace NavPosition
    deriving (Show)

data NavStatus = Idle 
               | DockingToStation StationID 
               | DockingToShip ShipID
               | Undocking
               | MovingTo { navMoving_velocity :: Vector3D
                          , navMoving_target :: Vector3D }
    deriving (Show)

data Ship = Ship
             { ship_name :: String
             , ship_class :: ShipClass
             , ship_stats :: ShipStats
             , ship_navModule :: NavModule
             , ship_AI :: ShipAI
             , ship_cargo :: Cargo
             , ship_owner :: OwnerID
             }
        deriving (Show)

defaultShips = I.fromList $ zip [0..] [rimbauld, goldenHind]

rimbauld :: Ship
rimbauld = defaultShip{ ship_name = "Rimbauld", ship_owner = 1 } -- 1 for Helen Ripley CARE

goldenHind :: Ship
goldenHind = defaultShip { ship_name = "Golden Hind", ship_owner = 1 } -- 1 for Helen Ripley CARE

defaultShip :: Ship
defaultShip = let defaultShipStats = shipStats defaultShipClass
              in Ship   "empty name"
                        defaultShipClass 
                        defaultShipStats 
                        defaultNavModule 
                        SAINone
                        defaultCargo 
                        defaultOwner

defaultNavModule = NavModule (DockedToStation 0) Idle

data ShipClass = Liandra -- small Anla'Shok vessel
               | Rhino -- Corvette-sized human freighter
               | WhiteStar -- Large League cruiser
               | Clark -- small human fighter-transport
               | Hel -- small minbari fighter-transport
               | GQuan -- small narn transport
               | Londo -- small narn fighter-transport
    deriving (Eq, Show)

defaultShipClass = Rhino

shipStats _ = ShipStats 1 10 100

defaultOwner = 0

firstEmptyKey :: IntMap a -> Int
firstEmptyKey = fst . findMax

defaultShipStats :: ShipClass -> ShipStats
defaultShipStats _ = ShipStats 1 10 100

undocking :: Ship -> Bool
undocking s = undockingStNS (navModule_status $ ship_navModule s)

docking :: Ship -> Bool
docking s = dockingStNS (navModule_status $ ship_navModule s)

dockingStID :: Ship -> StationID
dockingStID = dockingStNSID . navModule_status . ship_navModule

docked :: Ship -> Bool
docked s = dockedStNS (navModule_position $ ship_navModule s)

dockedStID :: Ship -> StationID
dockedStID = dockedStNSID . navModule_position . ship_navModule

dockedToSt :: Ship -> StationID -> Bool
dockedToSt = dockedToStNS . navModule_position . ship_navModule

dockingStNS :: NavStatus -> Bool
dockingStNS (DockingToStation _) = True
dockingStNS _ = False

undockingStNS :: NavStatus -> Bool
undockingStNS Undocking = True
undockingStNS _ = False

dockingStNSID :: NavStatus -> StationID
dockingStNSID (DockingToStation i) = i
dockingStNSID _ = undefined

dockedStNS :: ShipNavPosition -> Bool
dockedStNS (DockedToStation _) = True
dockedStNS _ = False

dockedStNSID :: ShipNavPosition -> StationID
dockedStNSID (DockedToStation i) = i
dockedStNSID _ = undefined

dockedToStNS :: ShipNavPosition -> StationID -> Bool
dockedToStNS (DockedToStation i) j = i == j
dockedToStNS _ _ = False

ship_freeSpace :: Ship -> Weight
ship_freeSpace s = fromIntegral( shipStats_cargoHold $ ship_stats s ) - weight (ship_cargo s)

tick = fromInteger tickGame -- WARNING: MAGIC CONSTANT
                            -- tickGame imported from GlobalConst

updateNavStatus :: NavModule -> NavModule -- ignores SpaceType FIXME
updateNavStatus m@(NavModule pos Idle) = m 
updateNavStatus m@(NavModule (SNPSpace (Space pos st)) (MovingTo vel targ)) =
    let posIfKeepMoving = pos + vel * tick
        closeEnough = length( targ-pos ) <= length( vel*tick )
    in if closeEnough then NavModule (SNPSpace (Space targ st)) Idle
                      else NavModule (SNPSpace (Space posIfKeepMoving st)) (MovingTo vel targ)

instance WareOps Ship where
    addWare w a st@Ship{ ship_cargo = cargo } = st{ ship_cargo = addWare w a cargo}
    enoughWare w a st@Ship{ ship_cargo = cargo } = enoughWare w a cargo
    checkWare w st@Ship{ ship_cargo = cargo } = checkWare w cargo
