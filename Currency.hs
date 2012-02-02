module Currency where

import Wrappers

defaultMoney = 300000

class MoneyOps a where
    addMoney :: a -> Amount -> a
    removeMoney :: a -> Amount -> a
    enoughMoney :: a -> Amount -> Bool
    mbRemoveMoney :: a -> Amount -> Maybe a

    removeMoney obj a = addMoney obj (-a)
    mbRemoveMoney obj a = 
        if (enoughMoney obj a) then Just $ removeMoney obj a
                               else Nothing
