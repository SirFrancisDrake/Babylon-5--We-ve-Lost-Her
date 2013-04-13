
{-# LANGUAGE ScopedTypeVariables #-}

module Babylon where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import System.Posix (sleep)

import DataTypes
import StartingInput
import Interface
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
  forkIO $ handle (\(_ :: SomeException ) -> putStrLn $ replicate 1000 'X') (gameCycleIO w stopLock)
  sleep 1
  runInterface w
  atomically $ writeTVar stopLock True
