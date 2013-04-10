module Auxiliary.Transactions where

import Control.Concurrent.STM
import Control.Monad (filterM)
import Control.Monad.Reader
import Data.IntMap

import Auxiliary.IntMap

modifyT :: (a -> a) -> (TVar a) -> STM ()
modifyT fn t = readTVar t >>= (writeTVar t) . fn

checkT :: (a -> b) -> (TVar a) -> STM b
checkT fn t = readTVar t >>= return . fn

liftToTVar = checkT

filterT :: (a -> Bool) -> [(TVar a)] -> STM [(TVar a)]
filterT p ts = filterM (\s -> checkT p s) ts

filterIT :: (a -> Bool) -> TVar (IntMap (TVar a)) -> STM [(TVar a)]
filterIT p its = readTVar its >>= (filterT p) . vals

mapIT :: (TVar a -> STM ()) -> (TVar (IntMap (TVar a))) -> STM ()
mapIT fn its = readTVar its >>= (mapM_ fn) . vals

mapITR :: (TVar a -> ReaderT b STM ()) -> (TVar (IntMap (TVar a))) -> ReaderT b STM ()
mapITR fn c = lift (readTVar c) >>= (mapM_ fn) . vals

-- filterT :: [(TVar a)] -> (a -> Bool) -> STM [(TVar a)]
-- filterT ts p = 
--   let fn = \acc tvar -> readTVar tvar >>= 
--              \var -> if p var then return (acc ++ [tvar])
--                               else return acc
--   in foldM fn [] ts

addInstance :: TVar (IntMap (TVar a)) -> a -> STM (TVar a)
addInstance instances newInstance = do
    instancesMap <- readTVar instances
    newInstanceTVar <- newTVar newInstance
    writeTVar instances $ insertMax newInstanceTVar instancesMap
    return newInstanceTVar

addInstanceTo :: TVar (IntMap (TVar a)) -> a -> Int -> STM (TVar a)
addInstanceTo instances newInstance i = do
    instancesMap <- readTVar instances
    newInstanceTVar <- newTVar newInstance
    writeTVar instances $ insert 0 newInstanceTVar instancesMap
    return newInstanceTVar

--addInstance_ :: TVar (IntMap (TVar a)) -> a -> STM ()
--addInstance_ instances newInstance = do
--    instancesMap <- readTVar instances
--    newInstanceTVar <- newTVar newInstance
--    writeTVar instances $ insertMax newInstanceTVar instancesMap

--addInstanceTo_ :: TVar (IntMap (TVar a)) -> a -> Int -> STM (TVar a)
--addInstanceTo_ instances newInstance i = atomically $ do
--    instancesMap <- readTVar instances
--    newInstanceTVar <- newTVar newInstance
--    writeTVar instances $ insert 0 newInstanceTVar instancesMap

listInstanceIDs :: TVar (IntMap (TVar a)) -> IO ()
listInstanceIDs instances = do
    instanceList <- readTVarIO instances
    putStrLn $ show $ keys instanceList

listInstances :: (Show a) => TVar (IntMap (TVar a)) -> IO ()
listInstances instances = do
    instanceList <- readTVarIO instances
    mapM_ (\k -> readTVarIO (instanceList ! k) >>= putStrLn . show ) 
          (keys instanceList)
