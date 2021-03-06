
{-# LANGUAGE FlexibleInstances #-}

module DataTypes where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Reader
import qualified Control.Monad.State as S
import Data.Function (on)
import Data.IntMap hiding (fromList, map)
import qualified Data.Map as M

import Auxiliary.IntMap
import Auxiliary.Transactions (liftToTVar)
import qualified Auxiliary.Zipper as Z
import Currency
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

data ShipAI = ShipAI (Z.Zipper SCommand)
            | SAIPlayer
            | SAINone
    deriving (Eq)

instance Show ShipAI where
  show _ = "someAI"

type SupplyPackage = [( Ware, Amount )]

data SCommand = 
  SGo (TVar Station) 
  | SBuy Ware Amount 
  | SSell Ware Amount
  | SLoad SupplyPackage
  | SUnload SupplyPackage
  deriving (Eq)

-- Encounters
data Encounter = Encounter
  { encounter_check :: ReaderT World STM Bool
  , encounter_chance :: Double
  , encounter_quest :: Quest
  }


-- Quests
data QVar =
  QInt Int
  | QBool Bool
  | QString String
  deriving (Eq, Show)

instance Ord QVar where
  (QInt a) <= (QInt b) = a <= b
  (<=) _ _ = error "Eq: QVar: incomparable types"

instance Num QVar where
  fromInteger i = QInt (fromInteger i)
  (+) _ _ = undefined
  (*) _ _ = undefined
  abs _ = undefined
  signum _ = undefined

data QScope =
  QSGlobal
  | QSLocal QuestName
  | QScope String
  deriving (Eq, Ord, Show)

type QuestVars = M.Map (QScope, QVarName) QVar

data Quest = Quest { q_title :: String
                   , q_screens :: Z.Zipper Screen
                   }

data Screen = Screen { s_id :: Int
                     , s_descr :: String
                     , s_actions :: [Action]
                     }

data Action = Action
    { a_descrs       :: [String]
    , a_successCheck :: Reader QuestContext Bool
    , a_screenT      :: Int
    , a_modT         :: S.State QuestContext ()
    , a_screenF      :: Int
    , a_modF         :: S.State QuestContext ()
    }

data QuestContext = QuestContext
  { qc_questName :: QuestName
  , qc_variables :: QuestVars
  }
  deriving ()

-- OWNERS.HS

data Player = Player
  { player_owner          :: TVar Owner
  , player_selectedShip   :: TVar Ship
  , player_knownJumpgates :: [Jumpgate]
  , player_questVars      :: TVar QuestVars -- no need to keep it TVared FIXME
  }
  deriving ()

data Owner = Owner 
  { owner_name :: String
  , owner_stationsOwned :: [(TVar Station)]
  , owner_shipsOwned :: [(TVar Ship)]
  , owner_personalInfo :: PersonalInfo
  , owner_money :: Money }
  deriving ()


--
--- SECTION BREAK
-----------------------
---  CONTRACTS
--

data Pilot = PilotPlayer | PilotToon Toon

data Toon = Toon
  { toon_name :: String
  , toon_cash :: Money
  , toon_job  :: ToonJob
  }
  deriving ()

data ToonJob =
  ToonUnemployed
  | ToonEmployed
    { tj_contract :: TVar Contract
    }
  deriving ()

data Vacancy = Vacancy
  { vacancy_issuer :: TVar Owner
  , vacancy_payment :: VacancyPayment
  --, vacancy_status :: VacancyStatus
  }
  deriving ()

data VacancyPayment = VacPay_Fixed Int

data Contract = Contract
  { contract_issuer :: TVar Owner
  , contract_route  :: ContractRoute
  , contract_status :: ContractStatus
  , contract_length :: ContractLength
  } deriving ()

data ContractStatus =
    CS_Outstanding
  | CS_IsWorked { cs_iw_agent :: TVar Agent }
  deriving ()

data ContractLength =
    CL_Forever
  deriving ()

-- data ContractDetails = ContractCourier
--   { cc_pilot :: TVar Toon
--   , cc_route :: [ContractRouteNode] -- FIXME
--   , cc_payment :: ContractPayment
--   , cc_expires :: Int -- FIXME
--   }
--   deriving ()
-- 
-- data Contract = Contract
--   { contr_issuer :: TVar Owner
--   , contr_status :: ContractStatus
--   , contr_details :: ContractDetails
--   }
-- 
-- data ContractRouteNode =
--   ContractRouteNode
--   { crn_station :: TVar Station
--   , crn_purchaseList :: [ (Ware, Amount) ]
--   , crn_sellList :: [ (Ware, Amount) ]
--   }
--   deriving ()
-- 
-- data ContractPayment =
--   PaymentPerRun Money
--   | PaymentPerTicks Int Money
--   deriving ()

data PersonalInfo = 
  Corporation { pi_corp_awesomeness :: Int }
  | Person { pi_person_race :: Race
           , pi_person_career :: Career
           }
  | Nation { pi_nation_something :: Int }
    deriving (Eq)

-- SHIPS.HS

data NavModule = NavModule { navModule_position :: ShipNavPosition
                           , navModule_status :: NavStatus
                           , navModule_program :: NavProgram
                           }
    deriving ()

instance Show NavModule where
  show (NavModule pos stat prog) =
    let posD = case pos of
          (DockedToStation _) -> "Docked. "
          (SNPSpace p) -> "space, " ++ show p
          (OnJumpgate jg) -> "on jumpgate to " ++ jg_name jg
          (InHyperspaceBetween (jg1,d1) (jg2,d2)) ->
            "in hyperspace, " ++ show d1 ++ " to " ++ jg_name jg1 ++
            " jumpgate, " ++ show d2 ++ " to " ++ jg_name jg2 ++ "jumpgate"
        statD = case stat of
          Idle -> " Idle" ++ "\n"
          (DockingToStation _) -> "Docking" ++ "\n"
          (DockingToShip _) -> "Docking" ++ "\n"
          (Jumping _ t) -> "Jumping to " ++ show t ++ "\n"
          (MovingInHyperspace v jg) -> "Moving towards " ++ jg_name jg ++ 
            " jumpgate, " ++ show v ++ "kps" ++ "\n"
          (MovingToSpace v t) -> "Moving to " ++ show t ++ "\nVelocity: " ++ show v
        progD = show prog
    in "Position: " ++ posD ++ "\nStatus: " ++ statD ++ "\n" ++ progD


data NavAction =
  NA_MoveTo Vector3D
  | NA_Jump SpaceType
  | NA_MoveInHyper Jumpgate
  | NA_Dock (TVar Station)
  | NA_Undock
  deriving ()

instance Show NavAction where
  show (NA_Dock _) = "\n Dock"
  show (NA_MoveTo v) = "\n Move to: " ++ show v
  show (NA_MoveInHyper jg) = "\n Hypertravel to " ++ jg_name jg ++ " jumpgate"
  show (NA_Jump st) = "\n Jump to " ++ show st
  show _ = error "\n Show: NavAction: undefined"

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
                     | InHyperspaceBetween (Jumpgate, Double) (Jumpgate, Double)
                     | OnJumpgate Jumpgate
    deriving ()

data NavStatus = Idle 
               | DockingToStation (TVar Station) 
               | DockingToShip (TVar Ship)
               | Undocking
               | MovingToSpace { navMoving_velocity :: Vector3D
                               , navMoving_target :: Vector3D }
               | MovingInHyperspace { navMoving_hyperspaceSpeed :: Double
                                    , navMoving_targetJumpgate  :: Jumpgate
                                    }
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
             , ship_pilot :: Pilot
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

instance Show (Station -> Station) where
    show _ = "some (Station -> Station)"

instance WareOps Station where
    addWarePure w a st@Station{ station_stock = stock } = st{ station_stock = addWarePure w a stock}
    enoughWarePure w a st@Station{ station_stock = stock } = enoughWarePure w a stock
    checkWarePure w st@Station{ station_stock = stock } = checkWarePure w stock

instance MoneyOps Station where
    addMoneyPure amount st@Station{ station_money = m } = st{ station_money = m + amount }
    enoughMoneyPure amount st@Station{ station_money = m } = m >= amount

instance StockOps Station where
    stockBuyPrice Station{ station_stock = stock } w = stockBuyPrice stock w
    stockSellPrice Station{ station_stock = stock } w = stockSellPrice stock w

-- World.hs

data World = World
    { world_stations  :: TVar (IntMap (TVar Station)) 
    , world_ships     :: TVar (IntMap (TVar Ship)) 
    , world_owners    :: TVar (IntMap (TVar Owner)) 
    , world_toons     :: TVar [TVar Toon] -- let's migrate from IntMaps to Lists FIXME
    , world_jumpgates :: TVar (IntMap Jumpgate)
    , world_encounters:: [Encounter]
    , world_player    :: TVar Player
    , world_time      :: TVar Int
    , world_pauseLock :: MVar ()
    } deriving ()

type Owners = TVar (IntMap (TVar Owner))
type Ships = TVar (IntMap (TVar Ship))
type Stations = TVar (IntMap (TVar Station))

