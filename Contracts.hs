
module Contracts where

import Control.Concurrent.STM

import DataTypes
import Wares
import Wrappers

data ContractAmountKind =
    CPK_Fixed Amount
  | CPK_All

singleDelivery :: TVar Station -> TVar Station -> Ware -> ContractAmountKind -> STM (TVar Contract)
singleDelivery tsrc tdest w a = 
  newTVar ContractCourier
    { cc_pilot = undefined
    , cc_route = 
      [ ContractRouteNode tsrc  [(w,a)] []
      , ContractRouteNode tdest []      [(w,a)]
      ]
    , cc_payment = undefined
    , cc_expires = undefined
    }

-- Simple business:
-- get Nickel and Cadmium, put Battaries


-- Wish:
-- 1. business can create a contract with `vacancy open' for pilot and fixed payment
-- 2. business can create a contract with `vacancy open' and `payment negotiable'
-- 3. business can create one of the above for a trade route :: [ContractRouteNode]
-- 4. A pilot accepting a contract starts from the station closest to him
-- and forms a navigational route along the trade route
-- 5. checking if the ship's cargo hold is big enough to run the route
-- 6. If purchasing is involved, check that the budget can fulfill the purchases
-- 7. some form of cards allowing to get some goods at some place for free
  
