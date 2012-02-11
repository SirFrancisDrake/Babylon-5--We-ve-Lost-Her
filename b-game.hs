module Babylon where

import Control.Concurrent
import Control.Concurrent.STM

import StartingInput
import Interface
import World

main = do
    (own, shi) <- getStartingInput
    newWorld <- makeNewWorld own shi
    stopLock <- newTVarIO False
    forkedID <- forkIO $ gameCycleIO newWorld stopLock
    playerCycle newWorld
    atomically $ writeTVar stopLock True

playerCycle :: World -> IO ()
playerCycle = interfaceCycle
