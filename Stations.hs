
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
    , station_description :: String
    , station_stockChangers :: [(Station -> Station)] -- natural income
    }                                                 -- and production
    deriving (Show)

instance Eq Station where
    (==) = on (==) station_name

instance ContextualShow Station where
    contextShow (ContextStationGuest _, _) st =
        "You are on " ++ station_name st ++ ". " ++ station_description st
    contextShow (ContextStationOwner _, _) st =
        "You are on " ++ station_name st ++ ". " ++ station_description st
    contextShow _ _ = undefined
    
instance Show (Station -> Station) where
    show _ = ""

instance WareOps Station where
    addWare w a st@Station{ station_stock = stock } = st{ station_stock = addWare w a stock}
    enoughWare w a st@Station{ station_stock = stock } = enoughWare w a stock
    checkWare w st@Station{ station_stock = stock } = checkWare w stock

instance MoneyOps Station where
    addMoney amount st@Station{ station_money = m } = st{ station_money = m + amount }
    enoughMoney amount st@Station{ station_money = m } = m >= amount

instance StockOps Station where
    stockBuyPrice Station{ station_stock = stock } w = stockBuyPrice stock w
    stockSellPrice Station{ station_stock = stock } w = stockSellPrice stock w
