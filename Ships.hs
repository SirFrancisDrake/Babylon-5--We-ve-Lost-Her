module Ships where

import Control.Concurrent
import Control.Concurrent.STM
import Data.IntMap

import Cargo
import ShipStats

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

data ShipClass = Liandra | Rhino | WhiteStar
                deriving (Eq, Show)

data ShipOwner = SO_Player | SO_Race ID | SO_Station ID
                deriving (Eq, Show)

type ID = Int

type NavModule = Int -- FIXME

firstEmptyKey :: IntMap a -> Int
firstEmptyKey = fst . findMax

