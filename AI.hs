module AI where

import Currency
import Owners
import Stations
import Wares
import Wrappers

data ShipAI = ShipAI { zai_current :: SCommand
                     , zai_list :: [SCommand]
                     }
            | SAIPlayer
            | SAINone
    deriving (Show)

data SCommand = SGo StationID | SBuy Ware Amount | SSell Ware Amount
    deriving (Eq, Show)

              -- where2buy   where2sell
defSupplyAI :: StationID -> StationID -> Ware -> Amount -> ShipAI
defSupplyAI bid sid w a = ShipAI (SGo bid)
                                 [ (SBuy w a)
                                 , (SGo sid)
                                 , (SSell w a)
                                 ]

next :: ShipAI -> ShipAI
next (ShipAI c (x:xs)) = ShipAI x (xs ++ [c])
