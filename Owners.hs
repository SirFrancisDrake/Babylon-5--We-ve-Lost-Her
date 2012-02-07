module Owners where

import Data.IntMap

import Currency
import PersonalData
import Wrappers

data Owner = Owner 
    { owner_name :: String
    , owner_stationsOwned :: [StationID]
    , owner_shipsOwned :: [ShipID]
    , owner_personalInfo :: PersonalInfo
    , owner_money :: Money }
    deriving (Show)

ownerOne :: Owner
ownerOne = Owner "Helen Ripley" [] [] defaultPersonalInfo 1000

defaultOwners = fromList $ zip [0..] [ownerOne]

defaultPersonalInfo = Person Human Military

data PersonalInfo = Corporation { pi_corp_awesomeness :: Int 
                                } 
                  | Person { pi_person_race :: Race
                           , pi_person_career :: Career
                           }
    deriving (Eq, Show)

instance MoneyOps Owner where
    addMoney o@Owner{ owner_money = om } m = o{ owner_money = om + m}
    enoughMoney o@Owner{ owner_money = om } m = om >= m
