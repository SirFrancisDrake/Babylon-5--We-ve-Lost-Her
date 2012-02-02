module Owners where

import Currency
import Wrappers

data Owner = Owner 
    { owner_name :: String
    , owner_stationsOwned :: [StationID]
    , owner_shipsOwned :: [ShipID]
    , owner_money :: Money }
    deriving (Show)

instance MoneyOps Owner where
    addMoney o@Owner{ owner_money = om } m = o{ owner_money = om + m}
    enoughMoney o@Owner{ owner_money = om } m = om >= m
