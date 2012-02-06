module Owners where

import Data.IntMap

import Currency
import Wrappers

data Owner = Owner 
    { owner_name :: String
    , owner_stationsOwned :: [StationID]
    , owner_shipsOwned :: [ShipID]
    , owner_money :: Money }
    deriving (Show)

ownerOne :: Owner
ownerOne = Owner "Helen Ripley" [] [] 1000

defaultOwners = fromList $ zip [0..] [ownerOne]

instance MoneyOps Owner where
    addMoney o@Owner{ owner_money = om } m = o{ owner_money = om + m}
    enoughMoney o@Owner{ owner_money = om } m = om >= m
