module Ships where

import Control.Concurrent
import Control.Concurrent.STM
import Data.IntMap

import GlobalTVars
import IntMapAux
import Navigation
import ShipStats
import Wares
import Wrappers

type Ships = IntMap (TVar Ship)

ships :: IO (TVar Ships)
ships = newTVarIO empty 

modifyShip :: Int -> Ships -> (Ship -> Ship) -> IO ()
modifyShip i ships fn = atomically $ do
    let shipVar = ships ! i
    ship <- readTVar shipVar
    writeTVar shipVar (fn ship) 

data Ship = Ship
             { ship_class :: ShipClass
             , ship_stats :: ShipStats
             , ship_navModule :: NavModule
             , ship_cargo :: Cargo
             , ship_owner :: ShipOwner
             , ship_ai :: AI
             }
        deriving (Show)

defaultShip :: Ship
defaultShip = let defaultShipStats = shipStats defaultShipClass
              in Ship   defaultShipClass 
                        defaultShipStats 
                        defaultNavModule 
                        defaultCargo 
                        defaultOwner
                        defaultAI

data ShipClass = Liandra | Rhino | WhiteStar
                deriving (Eq, Show)

defaultShipClass = Rhino

shipStats _ = ShipStats 1 10 100

data ShipOwner = SO_Station StationID | None
                deriving (Eq, Show)

defaultOwner = None

type AI = String -- FIXME
defaultAI = "none" -- FIXME

firstEmptyKey :: IntMap a -> Int
firstEmptyKey = fst . findMax

defaultShipStats :: ShipClass -> ShipStats
defaultShipStats _ = ShipStats 1 10 100
