module Babylon where

import Control.Concurrent
import Control.Concurrent.STM
import System.Posix (sleep)

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

-- for easy debugging
magic = debStartingInput >>= uncurry makeNewWorld

main = do
  w <- magic
  stopLock <- newTVarIO False
  pauseLock <- newMVar ()
  forkIO $ gameCycleIO w stopLock pauseLock
  sleep 1
  navTravelW w pauseLock
  atomically $ writeTVar stopLock True
