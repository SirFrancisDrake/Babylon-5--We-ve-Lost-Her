module Auxiliary.STM where

import Control.Concurrent.STM

liftToTVar :: (a -> b) -> TVar a -> STM b
liftToTVar fn tvar = readTVar tvar >>= return . fn
