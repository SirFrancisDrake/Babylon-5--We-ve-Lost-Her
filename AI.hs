module AI where

import Character
import Ships
import Wares
import Wrappers

data AI = AI [State] [Trigger]
    deriving ()

data State = Ok | NotOk
    deriving (Eq, Show)

type Trigger = (State -> Ship -> Character -> (State, Command))

data Command = Buy Ware Amount | Abstain
    deriving (Eq, Show)

resupplyFuel :: Trigger
resupplyFuel _ ship char =
    let fuel = snd $ head $ filter (\a -> fst a == Fuel) $ ship_cargo ship
    in if fuel < 100 then (Ok, Buy Fuel (100 - fuel))
                     else (Ok, Abstain)

-- commitAIAction :: Command -> Ship -> Character -> ????
-- no idea how to write this yet
