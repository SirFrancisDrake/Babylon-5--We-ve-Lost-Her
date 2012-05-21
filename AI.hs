module AI where

import Control.Concurrent.STM

import Currency
import DataTypes
import Owner
import Wares
import Wrappers

              -- where2buy   where2sell
defSupplyAI :: (TVar Station) -> (TVar Station) -> Ware -> Amount -> ShipAI
defSupplyAI btst stst w a = ShipAI (SGo btst)
                                   [ (SBuy w a)
                                   , (SGo stst)
                                   , (SSell w a)
                                   ]

defaultAI = SAINone

next :: ShipAI -> ShipAI
next (ShipAI c (x:xs)) = ShipAI x (xs ++ [c])
