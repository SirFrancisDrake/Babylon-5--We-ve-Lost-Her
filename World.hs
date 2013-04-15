
module World where

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent
import Control.Concurrent.STM hiding (check)
import Control.Monad (forM)
import Control.Monad.Reader
import qualified Data.Map as M
import Data.Maybe (isJust, fromJust)
import Data.IntMap hiding (filter, null, map)   -- to avoid confusion when
import qualified Data.IntMap as I (filter, map) -- using filter and map,
import Prelude hiding (filter, map)             -- I import them qualified,
import qualified Prelude as P (filter, map)     -- 'cuz it cost me 20 minutes once
import Data.List (foldl')  -- non-alphabetical so that I could write the above comment
import System.Console.ANSI
import System.Posix (sleep) -- first non-cross-platform line in the code

import AI
import Auxiliary.Concurrent
import Auxiliary.IntMap
import Auxiliary.StringFunctions
import Auxiliary.Transactions
import Auxiliary.Tuples
import Contexts
import Currency
import DataTypes
import Data.Everything
import GlobalConst
import Interface
import Jumpgates
import Navigation
import NavigationIO
import Owner
import Parsable
import Ships
import Space
import Stock
import TradeIO
import Wares
import World.Dump
import WorldGenerator
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
-- [PROCESS] - functions that process one particular `global` IntMap
--             like, update station tax revenues, burn ships fuel and so on
--             several auxiliary fns included


-- SECTION BREAK
-- [WORLD] -- see section description in the contents above

makeNewWorld :: Owner -> Ship -> IO World
makeNewWorld owner ship = do
    w <- atomically generateWorld
    pown <- atomically $ addInstance (world_owners w) owner
    psh  <- atomically $ addInstance (world_ships w) ship{ ship_owner = pown }
    readTVarIO pown >>= \own -> 
      atomically $ writeTVar pown own{ owner_shipsOwned = [psh] }
    plyr <- newTVarIO (Player pown psh [])
    plock <- newMVar ()
    pause plock
    return w{ world_player = plyr
            , world_pauseLock = plock }

-- this fn also exists in WorldGenerator.hs FIXME    
intMapToTVarIntMap :: IntMap a -> STM (IntMap (TVar a))
intMapToTVarIntMap ias = mapM newTVar vals >>= return . fromList . (zip keys)
                         where keys = P.map fst (toList ias)
                               vals = P.map snd (toList ias)

          -- stop lock  pause lock
gameCycle :: TVar Bool -> ReaderT World IO ()
gameCycle slock = do
  cycleEverything
  liftIO $ sleep (fromIntegral tickReal)
  stop <- liftIO $ readTVarIO slock
  if not stop 
    then ask >>= liftIO . readMVar . world_pauseLock >> gameCycle slock
    else return ()

gameCycleIO :: World -> TVar Bool -> IO ()
gameCycleIO w slock = runReaderT (gameCycle slock) w


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


--
--- SECTION BREAK
--- [READFN] -- see section description in the contents above
--

cycleEverything :: ReaderT World IO ()
cycleEverything = do
    world <- ask
    let owners = world_owners world
    let ships = world_ships world
    let stations = world_stations world
    let time = world_time world
    -- lift $ putStrLn "Updating owners."
    processIT owners
    -- lift $ putStrLn "Updating stations."
    processIT stations 
    --lift $ putStrLn "Updating ships."
    processIT ships
    --lift $ putStrLn "Processing docking."
    processDocking
    --lift $ putStrLn "Processing undocking."
    processUndocking
    --lift $ putStrLn "Processing navigational programs."
    liftIO $ processNavPrograms ships world
    liftIO $ processJumping ships
    --liftIO $ putStrLn "Updating time."
    liftIO $ atomically $ modifyT (1+) time

--
--- SECTION BREAK
--- [PROCESS] -- see section description in the contents above
--

-- minimal declaration: processSTM
class Processable a where
    process :: TVar a -> ReaderT World IO ()
    processSTM :: TVar a -> ReaderT World STM ()
    process = stmRtoIoR . processSTM
    processIT :: TVar (IntMap (TVar a)) -> ReaderT World IO ()
    processIT = stmRtoIoR . (mapITR processSTM)

instance Processable Station where
    processSTM tst = 
      lift (readTVar tst) >>= lift . (writeTVar tst) . stationFns
      where stationFns = foldl' (.) id
                                [ (\st -> addMoneyPure 3000 st) -- example tax income
                                ]

instance Processable Owner where
    processSTM _ = return ()

instance Processable Ship where
    processSTM tsh = do
      sh <- lift $ readTVar tsh
      let nm = ship_navModule sh
      lift $ writeTVar tsh sh{ ship_navModule = tickMove nm }
      lift (checkT runByAI tsh) >>= \b -> if b                
        then processAI tsh
        else return ()


dockMissing = processDocking

processDocking :: ReaderT World IO ()
processDocking = ask >>= \w -> lift $ processDocking' (world_ships w) (world_stations w)

processDocking' :: Ships -> Stations -> IO ()
processDocking' tships tstations = atomically $ do
  needDocking <- filterIT docking tships
  dockingStations <- mapM (checkT dockingSt) needDocking
  mapM_ (uncurry dockShSt) (zip needDocking dockingStations)

dockShSt :: (TVar Ship) -> (TVar Station) -> STM ()
dockShSt tsh tst = do
  ship <- readTVar tsh
  writeTVar tsh ship{ ship_navModule = NavModule (DockedToStation tst) Idle [] } -- FIXME
  station <- readTVar tst
  writeTVar tst station{ station_dockingBay =
                          if tsh `notElem` (station_dockingBay station)
                            then (station_dockingBay station) ++ [tsh] 
                            else station_dockingBay station }

processUndocking :: ReaderT World IO ()
processUndocking = ask >>= \w -> lift $ processUndocking' (world_ships w) (world_stations w)

processUndocking' :: Ships -> Stations -> IO ()
processUndocking' tships tstations = atomically $ do
  needUndocking <- filterIT undocking tships
  undockingStations <- mapM (checkT dockingSt) needUndocking
  mapM_ (uncurry undockShSt) (zip needUndocking undockingStations)

undockShSt :: (TVar Ship) -> (TVar Station) -> STM ()
undockShSt tsh tst = do

  ship <- readTVar tsh
  shipPos <- checkT station_position tst >>= return . departureAround 
  let navm = ship_navModule ship
  writeTVar tsh ship{ ship_navModule = 
    navm{ navModule_position = SNPSpace shipPos 
        , navModule_status = Idle } }

  station <- readTVar tst
  writeTVar tst station{ station_dockingBay =
                          P.filter (/= tsh) (station_dockingBay station) }

processJumping :: Ships -> IO ()
processJumping tshs = 
  readTVarIO tshs >>= atomically . (filterT jumping) . vals >>= (mapM_ jumpSh)

jumpSh :: (TVar Ship) -> IO ()
jumpSh tsh = do
  np <- atomically (checkT (navModule_status . ship_navModule) tsh) >>= \b ->
    case b of
      (Jumping (JE_Jumpgate jg) t) -> jump jg t
      _ -> error "World: jumpSh: Ship is not jumping"
  atomically $ setNavPosition tsh np
  atomically $ setNavStatus tsh Idle

processNavPrograms :: TVar (IntMap (TVar Ship)) -> World -> IO ()
processNavPrograms shs w = atomically $ 
  runReaderT genNavContextR w >>= \nc ->  
  mapIT (\sh -> runReaderT (processNavProgram nc sh) w) shs

processNavProgram :: NavContext -> TVar Ship -> ReaderT World STM ()
processNavProgram ctxt tsh = do
  idle <- lift $ checkT isIdle tsh
  when idle $ do
    sh <- lift (readTVar tsh) 
    nsh <- lift $ runReaderT (tickNavProgram sh) ctxt
    lift $ writeTVar tsh nsh
 
setOnCourse :: (TVar Ship) -> (TVar Station) -> STM ()
setOnCourse = setShipOnCourse


--
--- SECTION BREAK
--- [CREATING THINGS]
--
