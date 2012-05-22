module World where

import Control.Concurrent
import Control.Concurrent.STM hiding (check)
import Control.Monad (forM)
import Control.Monad.Reader
import Data.IntMap hiding (filter, map)         -- to avoid confusion when
import qualified Data.IntMap as I (filter, map) -- using filter and map,
import Prelude hiding (filter, map)             -- I import them qualified,
import qualified Prelude as P (filter, map)     -- 'cuz it cost me 20 minutes once
import Data.List (foldl') -- non-alphabetical so that I could write the above comment
import System.Posix (sleep) -- first non-cross-platform line in the code

import AI
import Currency
import DataTypes
import Data.Everything
import GlobalConst
import IntMapAux
import Navigation
import Owner
import Ships
import ShipsAndStations
import StationsData
import Stock
import Transactions
import qualified Vector as V
import Wares
import Wrappers

-- File contents, [INDEX] stands for `search for the bracketed word 
--                                  to get to the section mentioned`
--
-- [WORLD] - World data type has been moved to DataTypes, 
--           function for world-making are in WorldGenerator.hs
--           wrappers for `global` IntMaps and some auxiliary functions are still here
--
-- [GLOBTVAR] - functions for dealing with stuff inside the `global` IntMaps
--              quite rarely used, tbh, but possibly useful
--
-- [READFN] - functions inside ReaderT World that can be used
--            inside the game cycle with just >>
--            note that all the really cool stuff happens here
--
-- [PRINT] - debug function. But it prints the whole world! Ain't that somethin'
--
-- [PROCESS] - functions that process one particular `global` IntMap
--             like, update station tax revenues, burn ships fuel and so on
--             several auxiliary fns included


-- SECTION BREAK
-- [WORLD] -- see section description in the contents above

makeNewWorld :: Owner -> Ship -> IO World
makeNewWorld owner ship = do
    tStations <- newTVarIO empty
    atomically $ intMapToTVarIntMap defaultStations >>= writeTVar tStations

    tShips <- newTVarIO empty
    addInstance tShips ship -- adds the player character's ship
    --mapM_ (addInstance tShips) (vals defaultShips)

    tOwners <- newTVarIO empty
    addInstance tOwners owner -- adds the player character
    --mapM_ (addInstance tOwners) (vals defaultOwners)

    fillOwnedShips tShips tOwners -- adds owned ships IDs to owner_shipsOwned
    dockMissing tShips tStations -- docks the ships that start docked

    return $ World tStations tShips tOwners

-- this fn also exists in WorldGenerator.hs FIXME    
intMapToTVarIntMap :: IntMap a -> STM (IntMap (TVar a))
intMapToTVarIntMap ias = mapM newTVar vals >>= return . fromList . (zip keys)
                         where keys = P.map fst (toList ias)
                               vals = P.map snd (toList ias)

          -- stop lock  pause lock
gameCycle :: TVar Bool -> MVar () -> ReaderT World IO ()
gameCycle slock plock = 
         liftIO (readMVar plock)
         >> liftIO (sleep (fromIntegral tickReal))
         >> cycleEverything
      -- >> printWorld
         >> (liftIO $ readTVarIO slock)
         >>= \stop -> if (not stop) then gameCycle slock plock
                                    else return ()

gameCycleIO :: World -> TVar Bool -> MVar () -> IO ()
gameCycleIO w slock plock = runReaderT (gameCycle slock plock) w


-- SECTION BREAK
-- [GLOBTVAR] -- see section description in the contents above

(!!!) :: TVar (IntMap (TVar a)) -> Int -> IO a
(!!!) tas i = readTVarIO tas >>= \imap -> readTVarIO (imap ! i)

modifyGlobalTVar :: (IntMap a -> IntMap a) -> TVar (IntMap a) -> IO ()
modifyGlobalTVar fn tas = atomically $
                          readTVar tas >>= \as ->
                          writeTVar tas (fn as)

-- inserts an entry into a TVar'ed IntMap
insertIntoGlobalTVar :: a -> TVar (IntMap a) -> IO ()
insertIntoGlobalTVar a tas = modifyGlobalTVar (insertMax a) tas

-- Kills every entry satisfying given predicate
deleteWithFromGlobalTVar :: (a -> Bool) -> TVar (IntMap a) -> IO ()
deleteWithFromGlobalTVar pred tas = modifyGlobalTVar (I.filter $ not . pred) tas

-- Applies modifier function to every entry satisfying given predicate
modifyInGlobalTVar :: (a -> Bool) -> (a -> a) -> TVar (IntMap a) -> IO ()
modifyInGlobalTVar predicate modifier tas =
    let mod = I.map (\a -> if predicate a then modifier a else a)
    in modifyGlobalTVar mod tas


-- SECTION BREAK
-- [READFN] -- see section description in the contents above

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
    liftIO $ processUndocking ships stations

-- printWorld :: ReaderT World IO ()
-- printWorld = do
--     world <- ask
--     liftIO $ putStrLn "\n\nShowing stations:"
--     liftIO $ putStrLn "Sorry, disabled for weird reasons. See Mar 5th, 21:02"
--     liftIO $ putStrLn "\n\nShowing ships:"
--     liftIO $ printClass (world_ships world)
--     liftIO $ putStrLn "\n\nShowing owners:"
--     liftIO $ printClass (world_owners world)


-- SECTION BREAK
-- [PRINT] -- see section description in the contents above

printClass :: (Show a) => TVar (IntMap (TVar a)) -> IO ()
printClass tmap = readTVarIO tmap >>=
    (mapM readTVarIO) . (P.map snd) . toList >>= mapM_ print 


-- SECTION BREAK
-- [PROCESS] -- see section description in the contents above

class Processable a where
    process :: TVar a -> IO ()

instance Processable Station where
    process tst = atomically $
        readTVar tst >>= (writeTVar tst) . stationFns
        where stationFns = foldl' (.) id
                                  [ (\st -> addMoney 3000 st) -- example tax income
                                  ]

instance Processable Owner where
    process _ = return ()

instance Processable Ship where
    process _ = return ()

cycleClass :: (Processable a) => TVar (IntMap (TVar a)) -> IO ()
cycleClass timap = readTVarIO timap >>= \imap -> 
                                 mapM_ (\k -> process (imap ! k)) (keys imap)

fillOwnedShips :: Ships -> Owners -> IO ()
fillOwnedShips sh o = atomically $ do
    ships <- readTVar sh
    pairs <- mapM readTVar (shVals ships) >>= 
                        return . (zip (shKeys ships)) . (P.map ship_owner) 
    owners <- readTVar o
    mapM_ (\(sid,oid) -> readTVar (owners ! oid) >>= \own ->
              writeTVar (owners ! oid) own{ owner_shipsOwned = 
                if sid `notElem` (owner_shipsOwned own) then
                                                owner_shipsOwned own ++ [sid]
                                                        else
                                                owner_shipsOwned own})
          pairs
    where shKeys ships = P.map fst (toList ships)
          shVals ships = P.map snd (toList ships)
    
-- Appendix-like remain. I let it stay for clearness its name provides just once
dockMissing :: Ships -> Stations -> IO ()
dockMissing = processDocking

processDocking :: Ships -> Stations -> IO ()
processDocking tships tstations = atomically $ do
    ships <- readTVar tships
    stations <- readTVar tstations
    needDocking <- filterM (\ship -> check ship docking) (vals ships)
    dockingStations <- forM needDocking (\ship -> check ship dockingSt)
    let dockingPairs = zip needDocking dockingStations
    mapM_ (\(ship,station) -> dockShSt ship station) dockingPairs

dockShSt :: (TVar Ship) -> (TVar Station) -> STM ()
dockShSt tsh tst = do
    ship <- readTVar tsh
    writeTVar tsh ship{ ship_navModule = NavModule (DockedToStation tst) Idle }
    station <- readTVar tst
    writeTVar tst station{ station_dockingBay =
                            if tsh `notElem` (station_dockingBay station)
                                    then  (station_dockingBay station) ++ [tsh] 
                                    else station_dockingBay station }

t :: Ships -> Stations -> (Ship -> Bool) ->
        ( (TVar Ship) -> (TVar Station) -> STM () ) -> STM ()
t tships tstations dockCheck dockFn = do
    ships <- readTVar tships
    stations <- readTVar tstations
    needDocking <- filterM (\ship -> check ship dockCheck) (vals ships)
    dockingStations <- forM needDocking (\ship -> check ship dockingSt)
    let dockingPairs = zip needDocking dockingStations
    mapM_ (\(ship,station) -> dockShSt ship station) dockingPairs

processUndocking :: Ships -> Stations -> IO ()
processUndocking tships tstations = atomically $ do
    ships <- readTVar tships
    stations <- readTVar tstations
    needUndocking <- filterM (\ship -> check ship undocking) (vals ships)
    undockingStations <- forM needUndocking (\ship -> check ship dockingSt)
    let undockingPairs = zip needUndocking undockingStations
    mapM_ (\(tsh,tst) -> undockShSt tsh tst) undockingPairs

undockShSt :: (TVar Ship) -> (TVar Station) -> STM ()
undockShSt tsh tst = do
    ship <- readTVar tsh
    station <- readTVar tst
    let departure = V.fromList [0.1, 0.1, 0.1] -- magic constant FIXME
    let stationPos = nav_pos_vec (station_position station)
    let newShipNavModule = NavModule (SNPSpace $ Space (stationPos + departure) 
                                                 Normalspace) 
                                     Idle 
    writeTVar tsh ship{ ship_navModule = newShipNavModule }
    writeTVar tst station{ station_dockingBay =
                            P.filter (/= tsh) (station_dockingBay station) }

setOnCourse :: (TVar Ship) -> (TVar Station) -> STM ()
setOnCourse tsh tst = readTVar tsh >>= (writeTVar tsh) . (flip setShipOnCourse tst)

processAI :: ShipAI -> (TVar Ship) -> ReaderT World IO ShipAI -- this fn does more than its 
processAI ai@(ShipAI (SGo tst) ais) tsh = do           -- type signature shows, care
    world <- ask
    ships <- liftIO $ readTVarIO $  world_ships world
    stations <- liftIO $ readTVarIO $ world_stations world
    satisfied <- liftIO $ readTVarIO tsh >>= return . (flip dockedToSt tst)
    if satisfied then return $ next ai
                 else return ai
processAI ai@(ShipAI (SBuy bw ba) ais) tsh = do
    world <- ask
    sh <- liftIO $ readTVarIO tsh
    let tst = dockedSt sh
    let to = ship_owner sh

    cb <- liftIO $ canBuy to tsh tst world bw ba
    if cb then do liftIO $ buy to tsh tst world bw ba
                  return $ next ai
          else return ai
processAI ai@(ShipAI (SSell sw sa) ais) shid = do
    world <- ask
    sh <- liftIO $ (world_ships world) !!! shid
    let stid = dockedSt sh
    let oid = ship_owner sh
    let enoughWareP = enoughWare sw sa sh

    cs <- liftIO $ canSell oid shid stid world sw sa

    if cs then do liftIO $ sell oid shid stid world sw sa
                  return $ next ai
          else if enoughWareP then return $ next ai -- if AI doesn't have the cargo,
                              else return ai -- it should move on to its next task
                       -- otherwise it should wait for station to get enough money
processAI _ _ = undefined

canBuy :: OwnerID -> ShipID -> StationID -> World -> Ware -> Amount -> IO Bool
canBuy oid shid stid w bw ba = do
    sh <- (world_ships w) !!! shid
    st <- (world_stations w) !!! stid
    o <- (world_owners w) !!! oid

    let enoughSpaceP = ship_freeSpace sh >= weight bw * fromIntegral ba
    let enoughMoneyP = owner_money o >= stockSellPrice st bw * fromIntegral ba
    let enoughWareP = enoughWare bw ba st

    return $ and [enoughSpaceP, enoughMoneyP, enoughWareP] 

buy :: OwnerID -> ShipID -> StationID -> World -> Ware -> Amount -> IO ()
buy oid shid stid w bw ba = do
    st <- (world_stations w) !!! stid

    tsh <- readTVarIO (world_ships w) >>= \i -> return $ i ! shid
    tst <- readTVarIO (world_stations w) >>= \i -> return $ i ! stid
    to <- readTVarIO (world_owners w) >>= \i ->  return $i ! oid

    cb <- canBuy oid shid stid w bw ba

    if cb then atomically $ do
            stmRemoveWare bw ba tst
            stmAddWare bw ba tsh
            stmRemoveMoney (stockSellPrice st bw * fromIntegral ba) to
            stmAddMoney (stockSellPrice st bw * fromIntegral ba) tst
          else return ()

canSell :: OwnerID -> ShipID -> StationID -> World -> Ware -> Amount -> IO Bool
canSell oid shid stid w sw sa = do
    sh <- (world_ships w) !!! shid
    st <- (world_stations w) !!! stid

    let enoughMoneyP = station_money st >= stockBuyPrice st sw * fromIntegral sa
    let enoughWareP = enoughWare sw sa sh

    return $ and [enoughMoneyP, enoughWareP] 

sell :: OwnerID -> ShipID -> StationID -> World -> Ware -> Amount -> IO ()
sell oid shid stid w sw sa = do
    st <- (world_stations w) !!! stid

    tsh <- readTVarIO (world_ships w) >>= \i -> return $ i ! shid
    tst <- readTVarIO (world_stations w) >>= \i -> return $ i ! stid
    to <- readTVarIO (world_owners w) >>= \i ->  return $i ! oid

    cs <- canSell oid shid stid w sw sa

    if cs then atomically $ do
            stmRemoveWare sw sa tsh
            stmAddWare sw sa tst
            stmRemoveMoney (stockBuyPrice st sw * fromIntegral sa) tst
            stmAddMoney (stockBuyPrice st sw * fromIntegral sa) to
          else return ()

-- Following 2 fns are named counter-intuitive, since only one is invasive
-- still, I think their return types go well with their names
-- but anyways, these should be regarded as utility
stmPerform :: (a -> b) -> (TVar a) -> STM b
stmPerform fn tobj = readTVar tobj >>= return . fn

stmPerform_ :: (a -> a) -> (TVar a) -> STM ()
stmPerform_ fn tobj = readTVar tobj >>= (writeTVar tobj) . fn

-- WareOps for STM'ed instances of WareOps
stmCheckWare :: (WareOps a) => Ware -> (TVar a) -> STM Amount
stmCheckWare w = stmPerform (checkWare w)

stmEnoughWare :: (WareOps a) => Ware -> Amount -> (TVar a) -> STM Bool
stmEnoughWare w a = stmPerform (enoughWare w a)

stmAddWare :: (WareOps a) => Ware -> Amount -> (TVar a) -> STM ()
stmAddWare w a = stmPerform_ (addWare w a)

stmRemoveWare :: (WareOps a) => Ware -> Amount -> (TVar a) -> STM ()
stmRemoveWare w a = stmPerform_ (removeWare w a)

-- MoneyOps for STM'ed instances of MoneyOps
stmAddMoney :: (MoneyOps a) => Amount -> (TVar a) -> STM ()
stmAddMoney a = stmPerform_ (addMoney a)

stmRemoveMoney :: (MoneyOps a) => Amount -> (TVar a) -> STM ()
stmRemoveMoney a = stmPerform_ (removeMoney a)
