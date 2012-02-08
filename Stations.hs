module Stations where

import Control.Concurrent.STM
import Data.IntMap

import Currency
import IntMapAux
import Navigation
import qualified Vector as V
import Wares
import Wrappers

type Stations = IntMap (TVar Station)

data Station = Station
    { station_name :: String
    , station_position :: NavPosition
    , station_cargo :: Cargo
    , station_money :: Money
    , station_dockingBay :: [ShipID]
    , station_owner :: String
    , station_cargoChangers :: [(Station -> Station)] -- natural income
    }                                                 -- and production
    deriving (Show)

citadelStation :: Station
citadelStation = Station "Citadel station" 
                         (Space (V.fromList [0,0,0]) Normalspace)
                         defaultCargo 
                         defaultMoney 
                         [] 
                         "TriOptimum Corp."
                         [ (exchangeWareTimesStation Silicium 30 CyberModules 1 8)
                         ]

solarisOne :: Station
solarisOne = Station "Solaris research station" 
                     (Space (V.fromList [120,300,451]) Normalspace)
                     defaultCargo 
                     defaultMoney 
                     [] 
                     "Gibarian Inc."
                     [ (exchangeWareTimesStation Energy 300 Books 1 8)
                     ]

instance Show (Station -> Station) where
    show _ = ""

exchangeWareTimesStation :: Ware -> Amount -> Ware -> Amount -> Int -> Station -> Station
exchangeWareTimesStation wc ac wp ap t st = 
    let ca = station_cargo st
    in st{ station_cargo = exchangeWareTimes ca wc ac wp ap t }

defaultStations :: IntMap Station
defaultStations = fromList $ zip [0..] [citadelStation, solarisOne]

instance WareOps Station where
    addWare st@Station{ station_cargo = cargo } w a = st{ station_cargo = addWare cargo w a}
    enoughWare st@Station{ station_cargo = cargo } w a = enoughWare cargo w a
    checkWare st@Station{ station_cargo = cargo } w = checkWare cargo w

instance MoneyOps Station where
    addMoney st@Station{ station_money = m } amount = st{ station_money = m + amount }
    enoughMoney st@Station{ station_money = m } amount = m >= amount
