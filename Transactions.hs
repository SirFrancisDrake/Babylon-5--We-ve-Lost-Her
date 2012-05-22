module Transactions where

import Control.Concurrent.STM
import Data.IntMap

import Currency
import DataTypes
import IntMapAux
import Owner
import Wares
import Wrappers

-- DEPRECATED SINCE MAR 29th, 2011
-- modify :: IntMap (TVar a) -> Int -> (a -> a) -> STM ()
-- modify objs oid fn =
--     let objVar = objs ! oid
--     in readTVar objVar >>= \p -> writeTVar objVar (fn p)
-- 
-- check :: IntMap (TVar a) -> Int -> (a -> b) -> STM b
-- check objs oid fn =
--     let objVar = objs ! oid
--     in readTVar objVar >>= return . fn

modify :: (TVar a) -> (a -> a) -> STM ()
modify t fn = readTVar t >>= (writeTVar t) . fn

check :: (TVar a) -> (a -> b) -> STM b
check t fn = readTVar t >>= return . fn

-- example function
-- removes  ware   amount adds ware amount       to a station
exchange :: Ware -> Amount -> Ware -> Amount -> (TVar Station) -> IO ()
exchange rw ra aw aa station = atomically $ do
    modify station (removeWare rw ra)
    modify station (addWare aw aa)

addInstance :: TVar (IntMap (TVar a)) -> a -> IO ()
addInstance instances newInstance = atomically $ do
    instancesMap <- readTVar instances
    newInstanceTVar <- newTVar newInstance
    writeTVar instances $ insertMax newInstanceTVar instancesMap

addInstanceTo :: TVar (IntMap (TVar a)) -> a -> Int -> IO ()
addInstanceTo instances newInstance i = atomically $ do
    instancesMap <- readTVar instances
    newInstanceTVar <- newTVar newInstance
    writeTVar instances $ insert 0 newInstanceTVar instancesMap

listInstanceIDs :: TVar (IntMap (TVar a)) -> IO ()
listInstanceIDs instances = do
    instanceList <- readTVarIO instances
    putStrLn $ show $ keys instanceList

listInstances :: (Show a) => TVar (IntMap (TVar a)) -> IO ()
listInstances instances = do
    instanceList <- readTVarIO instances
    mapM_ (\k -> readTVarIO (instanceList ! k) >>= putStrLn . show ) 
          (keys instanceList)
