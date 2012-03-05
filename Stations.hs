
{-# LANGUAGE FlexibleInstances #-}

module Stations where

import Control.Concurrent.STM
import Data.Function (on)
import Data.IntMap

import Currency
import InterfaceShow
import IntMapAux
import Navigation
import Stock
import qualified Vector as V
import Wares
import Wrappers

type Stations = IntMap (TVar Station)

data Station = Station
    { station_name :: String
    , station_position :: NavPosition
    , station_stock :: Stock
    , station_money :: Money
    , station_dockingBay :: [ShipID]
    , station_owner :: String
    , station_stockChangers :: [(Station -> Station)] -- natural income
    }                                                 -- and production
    deriving (Show)

station_guestShow :: Station -> String
station_guestShow st = "Station " ++ station_name st ++ "\n" ++ show (station_stock st)

instance Eq Station where
    (==) = on (==) station_name

instance ContextualShow Station where
    contextShow (ContextStationGuest _, _) st =
        "Station " ++ station_name st ++ "\n" ++ show (station_stock st)
    contextShow (ContextStationOwner _, _) st =
        "Station " ++ station_name st ++ "\n" ++ show (station_stock st)
    contextShow _ _ = undefined
    
instance Show (Station -> Station) where
    show _ = ""

station_wareCost :: Station -> Ware -> Int
station_wareCost st w = 30

instance WareOps Station where
    addWare w a st@Station{ station_stock = stock } = st{ station_stock = addWare w a stock}
    enoughWare w a st@Station{ station_stock = stock } = enoughWare w a stock
    checkWare w st@Station{ station_stock = stock } = checkWare w stock

instance MoneyOps Station where
    addMoney amount st@Station{ station_money = m } = st{ station_money = m + amount }
    enoughMoney amount st@Station{ station_money = m } = m >= amount
