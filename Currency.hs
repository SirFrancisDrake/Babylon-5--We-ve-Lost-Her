module Currency where

import Wrappers

defaultMoney :: Int
defaultMoney = 300000

class MoneyOps a where
    addMoney :: Amount -> a -> a
    removeMoney :: Amount -> a -> a
    enoughMoney :: Amount -> a -> Bool
    mbRemoveMoney :: Amount -> a -> Maybe a

    removeMoney a obj = addMoney (-a) obj
    mbRemoveMoney a obj = 
        if (enoughMoney a obj) then Just $ removeMoney a obj
                               else Nothing
