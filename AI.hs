module AI where

import Character
import Currency
import Owners
import Ships hiding (AI(..))
import Stations
import Wares
import Wrappers

data ZippedAI = ZippedAI { zai_current :: ZCommand 
                         , zai_list :: [ZCommand]
                         }
    deriving (Show)

data ZCommand = ZGo StationID | ZBuy Ware Amount | ZSell Ware Amount
    deriving (Eq, Show)

              -- where2buy   where2sell
defProviderAI :: StationID -> StationID -> Ware -> Amount -> ZippedAI
defProviderAI bid sid w a = ZippedAI (ZGo bid)
                                   [ (ZBuy w a)
                                   , (ZGo sid)
                                   , (ZSell w a)
                                   ]

next :: ZippedAI -> ZippedAI
next (ZippedAI c (x:xs)) = ZippedAI x (xs ++ [c])

data AI = AI [State] [Trigger]
    deriving ()

type State = String

type Trigger = (State -> Ship -> Character -> (State, Command))

data Command = Buy Ware Amount | Idle
    deriving (Eq, Show)

exampleAI :: AI
exampleAI = AI [ "WaitForCargo"
               , "GoToSell"
               , "Sell"
               , "GoToBuy"
               , "Buy"
               ]
               [ \"GoToSell" sh ch -> ("Sell",Buy Fuel 100)]

resupplyFuel :: Trigger -- example, it's not actually operational
resupplyFuel _ ship char =
    let fuel = checkWare ship Fuel
    in if fuel < 100 then ("Ok", Buy Fuel (100 - fuel))
                     else ("Problem", Idle)

