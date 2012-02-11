
{-# LANGUAGE FlexibleInstances #-}

module Stations where

import Control.Concurrent.STM
import Data.Function (on)
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

instance Eq Station where
    (==) = on (==) station_name

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

station_wareCost :: Station -> Ware -> Int
station_wareCost st w = 30

instance WareOps Station where
    addWare w a st@Station{ station_cargo = cargo } = st{ station_cargo = addWare w a cargo}
    enoughWare w a st@Station{ station_cargo = cargo } = enoughWare w a cargo
    checkWare w st@Station{ station_cargo = cargo } = checkWare w cargo

instance MoneyOps Station where
    addMoney amount st@Station{ station_money = m } = st{ station_money = m + amount }
    enoughMoney amount st@Station{ station_money = m } = m >= amount
