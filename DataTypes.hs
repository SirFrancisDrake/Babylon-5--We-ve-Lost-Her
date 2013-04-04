
{-# LANGUAGE FlexibleInstances #-}

module DataTypes where

import Control.Concurrent
import Control.Concurrent.STM
import Data.Function (on)
import Data.IntMap hiding (fromList, map)

import Auxiliary.IntMap
import Auxiliary.Transactions (liftToTVar)
import Currency
import InterfaceShow
import Jumpgates
import Navigation
import PersonalData
import ShipStats
import Space
import Stock
import Vector hiding (fromList)
import qualified Vector as V (fromList)
import Wares
import Wrappers

-- AI.HS

data ShipAI = ShipAI { zai_current :: SCommand -- Why not Zipper SCommand?
                     , zai_list :: [SCommand]
                     }
            | SAIPlayer
            | SAINone
    deriving ()

data SCommand = SGo (TVar Station) | SBuy Ware Amount | SSell Ware Amount
    deriving (Eq)

-- OWNERS.HS

data Player = Player
  { player_owner          :: TVar Owner
  , player_selectedShip   :: TVar Ship
  , player_knownJumpgates :: [Jumpgate]
  }
  deriving ()

data Owner = Owner 
  { owner_name :: String
  , owner_stationsOwned :: [(TVar Station)]
  , owner_shipsOwned :: [(TVar Ship)]
  , owner_personalInfo :: PersonalInfo
  , owner_money :: Money }
  deriving ()

data PersonalInfo = 
  Corporation { pi_corp_awesomeness :: Int }
  | Person { pi_person_race :: Race
           , pi_person_career :: Career
           }
  | Nation { pi_nation_something :: Int }
    deriving (Eq, Show)

-- SHIPS.HS

data NavModule = NavModule { navModule_position :: ShipNavPosition
                           , navModule_status :: NavStatus
                           , navModule_program :: NavProgram
                           }
    deriving ()

data NavAction =
  NA_MoveTo Vector3D
  | NA_Jump SpaceType
  | NA_Dock (TVar Station)
  | NA_Undock
  deriving ()

type NavProgram = [NavAction]

np_jump :: Jumpgate -> SpaceType -> NavProgram
np_jump jg st =
  let pos = case st of
              Hyperspace  -> jg_normalV jg
              Normalspace -> jg_hyperV jg
  in [NA_MoveTo pos, NA_Jump st]

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
               | Jumping JumpEngine SpaceType
    deriving ()

data JumpEngine = JE_Jumpgate Jumpgate
                | JE_Self
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

instance SpaceObject ShipNavPosition where
  spacePosition snp =
    case snp of
      (SNPSpace v) -> v
      otherwise -> error "Can't locate a docked ship"

instance SpaceObject Ship where
  spacePosition sh =
    let NavModule nmp _ _ = ship_navModule sh
    in case nmp of
         (SNPSpace v) -> v
         otherwise -> error "Can't locate a docked ship"

data ShipClass = Liandra   -- small Anla'Shok vessel
               | Rhino     -- Corvette-sized human freighter
               | WhiteStar -- Large League cruiser
               | Clark     -- small human fighter-transport
               | Hel       -- small minbari fighter-transport
               | GQuan     -- small narn transport
               | Londo     -- small narn fighter-transport
               | Sharlin   -- Large Minbari War Cruiser
    deriving (Eq, Show)

instance WareOps Ship where
    addWarePure w a st@Ship{ ship_cargo = cargo } = st{ ship_cargo = addWarePure w a cargo}
    enoughWarePure w a st@Ship{ ship_cargo = cargo } = enoughWarePure w a cargo
    checkWarePure w st@Ship{ ship_cargo = cargo } = checkWarePure w cargo

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

stationNamePosPure :: Station -> ( String, NavPosition )
stationNamePosPure st = ( station_name st, station_position st )

stationNamePos :: (TVar Station) -> STM ( String, NavPosition )
stationNamePos = liftToTVar stationNamePosPure  
              -- liftToTVar :: (a -> b) -> TVar a -> STM b

instance SpaceObject Station where
  spacePosition = station_position

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
    addWarePure w a st@Station{ station_stock = stock } = st{ station_stock = addWarePure w a stock}
    enoughWarePure w a st@Station{ station_stock = stock } = enoughWarePure w a stock
    checkWarePure w st@Station{ station_stock = stock } = checkWarePure w stock

instance MoneyOps Station where
    addMoney amount st@Station{ station_money = m } = st{ station_money = m + amount }
    enoughMoney amount st@Station{ station_money = m } = m >= amount

instance StockOps Station where
    stockBuyPrice Station{ station_stock = stock } w = stockBuyPrice stock w
    stockSellPrice Station{ station_stock = stock } w = stockSellPrice stock w

-- World.hs

data World = World
    { world_stations  :: TVar (IntMap (TVar Station)) 
    , world_ships     :: TVar (IntMap (TVar Ship)) 
    , world_owners    :: TVar (IntMap (TVar Owner)) 
    , world_jumpgates :: TVar (IntMap Jumpgate)
    , world_player    :: TVar Player
    , world_time      :: TVar Int
    , world_pauseLock :: MVar ()
    } deriving ()

type Owners = TVar (IntMap (TVar Owner))
type Ships = TVar (IntMap (TVar Ship))
type Stations = TVar (IntMap (TVar Station))

