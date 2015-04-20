
module Tick where

import Control.Concurrent.STM
import Control.Monad (filterM, foldM)
import Data.Function (on)
import Data.List (foldl')

import DataTypes

updateWorld :: TVar ([a], [b]) -> STM ()
updateWorld tworld =
  let updateAgent = id
      updateProvince = id
      biapply fn1 fn2 (a,b) = (fn1 a, fn2 b)
  in  readTVar tworld 
      >>= return . (biapply (map updateAgent) (map updateProvince)) 
      >>= writeTVar tworld
