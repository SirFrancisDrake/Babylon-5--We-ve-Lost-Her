
module DataTypes where

import Control.Concurrent.STM

import Auxiliary.Zipper

data Province = Province
  { province_name        :: String
  , province_coords      :: (Float, Float)
  , province_stock       :: [(Ware, Amount)]
  , province_productions :: [TVar Production]
  }
  deriving ()

data Ware =
  Salt
  | Grain
  | Bread
  | Copper
  | Tin
  | Bronze
  | Sword
  deriving (Eq, Show)

data Production = Production
  { production_province  :: TVar Province
  , production_inputs    :: [(Ware, Amount)]
  , production_outputs   :: [(Ware, Amount)]
  , production_baseTicks :: Int
  , production_contracts :: [TVar Contract]
  }
  deriving ()

type Amount = Int

data Agent = Agent
  { agent_capacity :: Int 
  , agent_position :: (Float, Float)
  , agent_maxSpeed :: Float
  , agent_cargo    :: [(Ware, Amount)]
  , agent_state    :: AgentState
  }
  deriving ()

data AgentState =
    AS_WorkingContract 
      { as_wc_contract :: TVar Contract 
      }
  | AS_LookingForContract
  | AS_Idle
  deriving ()

data Contract = Contract
  { contract_status :: ContractStatus
  , contract_length :: ContractLength
  , contract_route  :: ContractRoute
  }
  deriving ()

data ContractRoute = ContractRoute
  { contractRoute_from   :: TVar Production
  , contractRoute_to     :: TVar Production
  , contractRoute_ware   :: Ware
  , contractRoute_amount :: Amount
  }
  deriving ()

data ContractStatus =
    CS_Worked { csw_agent :: TVar Agent }
  | CS_Outstanding
  deriving ()

data ContractLength =
    CL_Forever
 -- CL_Finite
  deriving ()

type ProvinceName = String

data World = World
  { world_provinces   :: [TVar Province]
  , world_productions :: [TVar Production]
  , world_agents      :: [TVar Agent]
  , world_contracts   :: [TVar Contract]
  } deriving ()
