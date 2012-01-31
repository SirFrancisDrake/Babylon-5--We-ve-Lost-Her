module Ships where

import Control.Concurrent
import Control.Concurrent.STM
import Data.IntMap

import Cargo
import IntMapAux
import Navigation
import ShipStats
import Wrappers

type Ships = IntMap Ship

ships :: IO (TVar Ships)
ships = newTVarIO empty 

data Ship = Ship
             { ship_class :: ShipClass
             , ship_stats :: ShipStats
             , ship_navModule :: NavModule
             , ship_cargo :: Cargo
             , ship_owner :: ShipOwner
             }
        deriving (Show)

defaultShip :: Ship
defaultShip = let defaultShipStats = shipStats defaultShipClass
              in Ship   defaultShipClass 
                        defaultShipStats 
                        defaultNavModule 
                        defaultCargo 
                        defaultOwner

data ShipClass = Liandra | Rhino | WhiteStar
                deriving (Eq, Show)

defaultShipClass = Rhino

shipStats _ = ShipStats 1 10 100

data ShipOwner = SO_Station StationID | None
                deriving (Eq, Show)

defaultOwner = None

firstEmptyKey :: IntMap a -> Int
firstEmptyKey = fst . findMax

defaultShipStats :: ShipClass -> ShipStats
defaultShipStats _ = ShipStats 1 10 100
