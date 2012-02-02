module Stations where

import Control.Concurrent.STM
import Data.IntMap

import Currency
import IntMapAux
import Wares
import Wrappers

type Stations = IntMap (TVar Station)

data Station = Station
    { station_name :: String
    , station_cargo :: Cargo
    , station_money :: Money
    , station_dockingBay :: [ShipID]
    , station_owner :: String
    } 
    deriving (Show)

citadelStation :: Station
citadelStation = Station "Citadel station" defaultCargo defaultMoney [1,2] "TriOptimum corp"

instance WareOps Station where
    addWare st@Station{ station_cargo = cargo } w a = st{ station_cargo = addWare cargo w a}
    enoughWare st@Station{ station_cargo = cargo } w a = enoughWare cargo w a

instance MoneyOps Station where
    addMoney st@Station{ station_money = m } amount = st{ station_money = m + amount }
    enoughMoney st@Station{ station_money = m } amount = m >= amount
