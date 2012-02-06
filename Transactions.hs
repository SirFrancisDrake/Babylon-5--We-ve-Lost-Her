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
    let station' = removeWare station fw fa
    let station'' = addWare station' sw sa
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

-- Remove 30 Credits, add 15 Fuel, if not enough Credits, do nothing
-- Try the above variant, if it fails, remove (credits / 5), add fuel respectively
-- Same as 1 and 2, but with multiple wares to add and remove
-- purchase :: Station -> Owner -> Ware -> Amount -> STM ()
-- purchase station owner ware amount = 
--     let wareTotalCost = wareCost stat ware
--         wareAmount = wareAmount stat ware
--         doIWantThis = (wareAmount >= amount) && (enoughMoney own wareTotalCost)
--     in if (not doIWantThis) then return ()
--                             else do removeMoney own wareTotalCost
--                                     removeWare ware amount stat
--                                     addWare ware amount own
--                                     return ()
