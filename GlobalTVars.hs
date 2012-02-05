module GlobalTVars where

import Control.Concurrent.STM
import Control.Monad.Reader
import Data.IntMap
import Prelude hiding (filter, map)

import IntMapAux
import Owners
import Ships
import Stations

data World = World
    { world_stations :: TVar (IntMap (TVar Station)) 
    , world_ships :: TVar (IntMap (TVar Ship)) 
    , world_owners :: TVar (IntMap (TVar Owner)) 
    } deriving ()

modifyGlobalTVar :: (IntMap a -> IntMap a) -> TVar (IntMap a) -> IO ()
modifyGlobalTVar fn tas = atomically $
                          readTVar tas >>= \as ->
                          writeTVar tas (fn as)

-- inserts an entry into a TVar'ed IntMap
insertIntoGlobalTVar :: a -> TVar (IntMap a) -> IO ()
insertIntoGlobalTVar a tas = modifyGlobalTVar (insertMax a) tas

-- Kills every entry satisfying given predicate
deleteWithFromGlobalTVar :: (a -> Bool) -> TVar (IntMap a) -> IO ()
deleteWithFromGlobalTVar pred tas = modifyGlobalTVar (filter $ not . pred) tas

-- Applies modifier function to every entry satisfying given predicate
modifyInGlobalTVar :: (a -> Bool) -> (a -> a) -> TVar (IntMap a) -> IO ()
modifyInGlobalTVar predicate modifier tas =
    let mod = map (\a -> if predicate a then modifier a else a)
    in modifyGlobalTVar mod tas

cycleEverything :: ReaderT World IO ()
cycleEverything = do
    world <- ask
    let owners = world_owners world
    let ships = world_ships world
    let stations = world_stations world
    liftIO $ cycleClass owners
    liftIO $ cycleClass stations 
    liftIO $ cycleClass ships 
    liftIO $ processDocking ships stations

class Processable a where
    process :: Int -> IntMap (TVar a) -> IO ()

instance Processable Station where
    process sId ss = atomically $
        readTVar (sId ss) >>= \station ->
        writeTVar (sId ss) (addMoney station 3000) -- example tax income

instance Processable Owner where
    process _ _ = return ()

instance Processable Ship where
    process _ _ = return ()

cycleClass :: (Processable a) => TVar (IntMap (TVar a)) -> IO ()
cycleClass timap = readTVarIO timap >>= \imap -> mapM_ (\k -> process k imap) (keys imap)

processDocking (TVar (IntMap (TVar Ship))) -> (TVar (IntMap (TVar Station))) -> IO ()
processDocking tships tstations = atomically $ do
    ships <- readTVar tships
    stations <- readTVar tstations
    
