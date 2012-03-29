module Transactions where

import Control.Concurrent.STM
import Data.IntMap

import Currency
import IntMapAux
import Owners
import Stations
import Wares
import Wrappers

modify :: IntMap (TVar a) -> Int -> (a -> a) -> STM ()
modify objs oid fn =
    let objVar = objs ! oid
    in readTVar objVar >>= \p -> writeTVar objVar (fn p)

check :: IntMap (TVar a) -> Int -> (a -> b) -> STM b
check objs oid fn =
    let objVar = objs ! oid
    in readTVar objVar >>= return . fn

-- example function
-- removes  ware   amount adds ware amount       to a list of stations     and station index
exchange :: Ware -> Amount -> Ware -> Amount -> TVar (IntMap (TVar Station)) -> Int -> IO ()
exchange fw fa sw sa stations stationID = atomically $ do
    stationList <- readTVar stations
    let stationTVar = stationList ! stationID
    station <- readTVar stationTVar
    let station' = removeWare fw fa station
    let station'' = addWare sw sa station'
    writeTVar stationTVar station''

addInstance :: TVar (IntMap (TVar a)) -> a -> IO ()
addInstance instances newInstance = atomically $ do
    instancesMap <- readTVar instances
    newInstanceTVar <- newTVar newInstance
    writeTVar instances $ insertMax newInstanceTVar instancesMap

listInstanceIDs :: TVar (IntMap (TVar a)) -> IO ()
listInstanceIDs instances = do
    instanceList <- readTVarIO instances
    putStrLn $ show $ keys instanceList

listInstances :: (Show a) => TVar (IntMap (TVar a)) -> IO ()
listInstances instances = do
    instanceList <- readTVarIO instances
    mapM_ (\k -> readTVarIO (instanceList ! k) >>= putStrLn . show ) 
          (keys instanceList)
