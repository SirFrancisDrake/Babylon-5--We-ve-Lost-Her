
{-# LANGUAGE FlexibleInstances #-}

module DataTypes where

import Control.Concurrent.STM
import Data.Function (on)
import Data.IntMap hiding (fromList, map)

import Currency
import InterfaceShow
import IntMapAux
import Navigation
import PersonalData
import ShipStats
import Stock
import Vector hiding (fromList)
import qualified Vector as V (fromList)
import Wares
import Wrappers

-- AI.HS

data ShipAI = ShipAI { zai_current :: SCommand
                     , zai_list :: [SCommand]
                     }
            | SAIPlayer
            | SAINone
    deriving ()

data SCommand = SGo (TVar Station) | SBuy Ware Amount | SSell Ware Amount
    deriving (Eq)

-- OWNERS.HS

data Owner = Owner 
    { owner_name :: String
    , owner_stationsOwned :: [(TVar Station)]
    , owner_shipsOwned :: [(TVar Ship)]
    , owner_personalInfo :: PersonalInfo
    , owner_money :: Money }
    deriving ()

data PersonalInfo = Corporation { pi_corp_awesomeness :: Int 
                                } 
                  | Person { pi_person_race :: Race
                           , pi_person_career :: Career
                           }
                  | Nation { pi_nation_something :: Int
                           }
    deriving (Eq, Show)

-- SHIPS.HS

data NavModule = NavModule { navModule_position :: ShipNavPosition
                           , navModule_status :: NavStatus
                           }
    deriving ()

data ShipNavPosition = DockedToStation (TVar Station)
                     | DockedToShip (TVar Ship)
                     | SNPSpace NavPosition
    deriving ()

data NavStatus = Idle 
               | DockingToStation (TVar Station) 
               | DockingToShip (TVar Ship)
               | Undocking
               | MovingToSpace { navMoving_velocity :: Vector3D
                               , navMoving_target :: Vector3D }
               | MovingToStation (TVar Station)
    deriving ()

data Ship = Ship
             { ship_name :: String
             , ship_class :: ShipClass
             , ship_stats :: ShipStats
             , ship_navModule :: NavModule
             , ship_AI :: ShipAI
             , ship_cargo :: Cargo
             , ship_owner :: (TVar Owner)
             }
        deriving ()

data ShipClass = Liandra -- small Anla'Shok vessel
               | Rhino -- Corvette-sized human freighter
               | WhiteStar -- Large League cruiser
               | Clark -- small human fighter-transport
               | Hel -- small minbari fighter-transport
               | GQuan -- small narn transport
               | Londo -- small narn fighter-transport
               | Sharlin -- Large Minbari War Cruiser
    deriving (Eq, Show)

instance WareOps Ship where
    addWare w a st@Ship{ ship_cargo = cargo } = st{ ship_cargo = addWare w a cargo}
    enoughWare w a st@Ship{ ship_cargo = cargo } = enoughWare w a cargo
    checkWare w st@Ship{ ship_cargo = cargo } = checkWare w cargo

-- STATIONS.HS

data Station = Station
    { station_name :: String
    , station_position :: NavPosition
    , station_stock :: Stock
    , station_money :: Money
    , station_dockingBay :: [ TVar Ship ]
    , station_owner :: TVar Owner
    , station_description :: String
    , station_stockChangers :: [(Station -> Station)] -- natural income
    }                                                 -- and production
    deriving ()

instance Eq Station where
    (==) = on (==) station_name

instance ContextualShow Station where
    contextShow (ContextStationGuest _, _) st =
        "You are on " ++ station_name st ++ ". " ++ station_description st
    contextShow (ContextStationOwner _, _) st =
        "You are on " ++ station_name st ++ ". " ++ station_description st
    contextShow _ _ = undefined
    
instance Show (Station -> Station) where
    show _ = "some (Station -> Station)"

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

-- World.hs

data World = World
    { world_stations :: TVar (IntMap (TVar Station)) 
    , world_ships :: TVar (IntMap (TVar Ship)) 
    , world_owners :: TVar (IntMap (TVar Owner)) 
    } deriving ()

type Owners = TVar (IntMap (TVar Owner))
type Ships = TVar (IntMap (TVar Ship))
type Stations = TVar (IntMap (TVar Station))

