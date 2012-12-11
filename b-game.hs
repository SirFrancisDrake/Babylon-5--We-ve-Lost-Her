module Babylon where

import Control.Concurrent
import Control.Concurrent.STM

import DataTypes
import StartingInput
import Interface
import InterfaceShow
import World

-- main = do
--     (owner, ship) <- getStartingInput
--     newWorld <- makeNewWorld owner ship
--     stopLock <- newTVarIO False
--     pauseLock <- newMVar ()
--     forkedID <- forkIO $ gameCycleIO newWorld stopLock pauseLock
--     playerCycle newWorld
--     atomically $ writeTVar stopLock True
-- 
-- playerCycle :: World -> IO ()
-- playerCycle w = interfaceCycle w startingIState
