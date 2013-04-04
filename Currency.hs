module Currency where

import Control.Concurrent.STM

import Auxiliary.Transactions
import Wrappers

defaultMoney :: Int
defaultMoney = 300000

class MoneyOps a where
    addMoneyPure    :: Amount -> a -> a
    removeMoneyPure :: Amount -> a -> a
    enoughMoneyPure :: Amount -> a -> Bool
    mbRemoveMoney   :: Amount -> a -> Maybe a

    addMoney    :: Amount -> TVar a -> STM ()
    addMoney = modifyT . addMoneyPure
    removeMoney :: Amount -> TVar a -> STM ()
    removeMoney = modifyT . removeMoneyPure
    enoughMoney :: Amount -> TVar a -> STM Bool
    enoughMoney = checkT . enoughMoneyPure

    removeMoneyPure a obj = addMoneyPure (-a) obj
    mbRemoveMoney a obj = 
        if (enoughMoneyPure a obj) 
          then Just $ removeMoneyPure a obj
          else Nothing
