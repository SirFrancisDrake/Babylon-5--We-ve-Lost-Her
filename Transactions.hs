module Transactions where

import Control.Concurrent.STM
import Control.Monad (filterM)
import Data.IntMap

import Auxiliary.IntMap
import Currency
import DataTypes
import Owner
import Wares
import Wrappers

modifyT :: (a -> a) -> (TVar a) -> STM ()
modifyT fn t = readTVar t >>= (writeTVar t) . fn

checkT :: (a -> b) -> (TVar a) -> STM b
checkT fn t = readTVar t >>= return . fn

filterT :: (a -> Bool) -> [(TVar a)] -> STM [(TVar a)]
filterT p ts = filterM (\s -> checkT p s) ts

filterIT :: (a -> Bool) -> TVar (IntMap (TVar a)) -> STM [(TVar a)]
filterIT p its = readTVar its >>= (filterT p) . vals

-- filterT :: [(TVar a)] -> (a -> Bool) -> STM [(TVar a)]
-- filterT ts p = 
--   let fn = \acc tvar -> readTVar tvar >>= 
--              \var -> if p var then return (acc ++ [tvar])
--                               else return acc
--   in foldM fn [] ts

-- example function
-- removes  ware   amount adds ware amount       to a station
exchange :: Ware -> Amount -> Ware -> Amount -> (TVar Station) -> IO ()
exchange rw ra aw aa station = atomically $ do
    modifyT (removeWare rw ra) station
    modifyT (addWare aw aa) station

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
