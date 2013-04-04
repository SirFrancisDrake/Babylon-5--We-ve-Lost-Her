module Transactions where

import Control.Concurrent.STM

import Auxiliary.Transactions
import DataTypes
import Wares
import Wrappers

exchange :: Ware -> Amount -> Ware -> Amount -> (TVar Station) -> STM ()
exchange rw ra aw aa station = do
    modifyT (removeWare rw ra) station
    modifyT (addWare aw aa) station

