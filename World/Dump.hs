
module World.Dump where

import Control.Concurrent.STM
import Control.Monad.Reader

import Auxiliary.IntMap
import Auxiliary.StringFunctions
import DataTypes

dumpShips :: Ships -> STM [String]
dumpShips tshs =
  let fn :: TVar Ship -> STM String
      fn tsh = do 
        sh <- readTVar tsh 
        n <- readTVar (ship_owner sh) >>= return . owner_name
        return $ concatWith "\n "
          [ ship_name sh
          , show (ship_class sh)
          , show (ship_stats sh)
          , show (ship_navModule sh)
          , show (ship_AI sh)
          , show (ship_cargo sh)
          , n ] 
  in readTVar tshs >>= (mapM fn) . vals
