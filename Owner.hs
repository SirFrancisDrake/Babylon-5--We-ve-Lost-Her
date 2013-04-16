module Owner where

import Data.IntMap

import Currency
import DataTypes
import PersonalData
import Wrappers

instance Show Owner where
    show t = show (owner_name t)

ownerOne :: Owner
ownerOne = Owner "Helen Ripley" [] [] defaultPersonalInfo 1000

defaultOwners = fromList $ zip [1..] [ownerOne]

defaultPersonalInfo = Person Human Military

instance MoneyOps Owner where
    addMoneyPure m o@Owner{ owner_money = om } = o{ owner_money = om + m}
    enoughMoneyPure m o@Owner{ owner_money = om } = om >= m

