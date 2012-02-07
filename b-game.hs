module Babylon where

import Control.Concurrent
import Control.Concurrent.STM

import StartingInput
import World

main = do
    (own, shi) <- getStartingInput
    newWorld <- makeNewWorld own shi
    stopLock <- newTVarIO False
    forkedID <- forkIO $ gameCycleIO newWorld stopLock
    playerCycle

playerCycle = undefined
