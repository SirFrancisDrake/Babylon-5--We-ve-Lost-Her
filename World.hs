
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
import ShipsAndStations
import Space
import Stock
import Wares
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
-- [PRINT] - debug function. But it prints the whole world! Ain't that somethin'
--
-- [PROCESS] - functions that process one particular `global` IntMap
--             like, update station tax revenues, burn ships fuel and so on
--             several auxiliary fns included


-- SECTION BREAK
-- [WORLD] -- see section description in the contents above

pause :: MVar () -> IO ()
pause a = putStrLn "Pausing. " >> takeMVar a

unpause :: MVar () -> IO ()
unpause a = putStrLn "Unpausing. " >> putMVar a ()

makeNewWorld :: Owner -> Ship -> IO World
makeNewWorld owner ship = do
    w <- atomically generateWorld
    pown <- atomically $ addInstance (world_owners w) owner
    psh  <- atomically $ addInstance (world_ships w) ship
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


-- SECTION BREAK
-- [READFN] -- see section description in the contents above

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

-- SECTION BREAK
-- [PRINT] -- see section description in the contents above

printClass :: (Show a) => TVar (IntMap (TVar a)) -> IO ()
printClass tmap = readTVarIO tmap >>=
    (mapM readTVarIO) . (P.map snd) . toList >>= mapM_ print 


-- SECTION BREAK
-- [PROCESS] -- see section description in the contents above

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

-- Appendix-like remain. I let it stay for clearness its name provides just once
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
processNavPrograms shs w = runReaderT genNavContext w >>= \nc -> atomically $ 
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

setNavProgram :: (TVar Ship) -> NavProgram -> STM ()
setNavProgram tsh prg = modifyT (setNavProgramPure prg) tsh

runInterface :: World -> IO ()
runInterface w = 
  setCursorPosition 0 0 >> clearFromCursorToScreenEnd >> setCursorPosition 5 0 
  >> runReaderT interface w

-- To link together `ReaderT World IO (TVar sth)' and `ReaderT World STM (TVar sth)'
-- one can pass an accessor (readTVarIO and readTVar respectively) into a reader:
-- getPlayerWith accessor = 
--   ask >>= lift . accessor . world_owners >>= return . (\a -> a ! 0)
--
-- This is the type signature, borrowing from ghci:
-- getPlayerWith
--  :: ( EnvType (t, m) ~ World
--     , MonadReader (t, m)
--     , MonadTrans t
--     , Monad m) =>
--     (TVar (IntMap (TVar Ship)) -> m (IntMap b)) -> t m b

stmRtoIoR :: ReaderT a STM r -> ReaderT a IO r
stmRtoIoR r1 = ask >>= liftIO . atomically . (runReaderT r1)

getPlayerShipSTM :: ReaderT World STM (TVar Ship)
getPlayerShipSTM = ask >>= lift . readTVar . world_player >>= return . player_selectedShip

getPlayerSTM :: ReaderT World STM (TVar Owner)
getPlayerSTM = ask >>= lift . readTVar . world_player >>= return . player_owner

getPlayerShipIO :: ReaderT World IO (TVar Ship)
getPlayerShipIO = stmRtoIoR getPlayerShipSTM

getPlayerIO :: ReaderT World IO (TVar Owner)
getPlayerIO = stmRtoIoR getPlayerSTM

data Menu_ActionAfter =
  MAA_Depends
  | MAA_Finish
  | MAA_Return
  deriving ()

data Menu_Result =
  MR_Pop
  | MR_Stay
  | MR_Top
  deriving ()
  
type Menu a = M.Map String (ReaderT a STM Bool, ReaderT a IO Menu_Result, Menu_ActionAfter)

processMenu :: Menu a -> ReaderT a IO ()
processMenu menu = do
  context <- ask
  filteredOptionsList <- filterM (\(_,(b,_,_)) -> stmRtoIoR b) (M.toList menu)
  let filteredMenu = M.fromList (P.map (\(a,(_,c,d)) -> (a,(c,d))) filteredOptionsList)
  a <- liftIO $ getByNum (M.keys filteredMenu)
  lift $ setCursorPosition 0 0 >> clearFromCursorToScreenEnd >> setCursorPosition 5 0
  liftIO $ putStrLn $ "> " ++ a ++ "\n"
  result <- fst (filteredMenu M.! a)
  case result of
    MR_Stay -> processMenu menu
    otherwise ->
      case snd (filteredMenu M.! a) of
        MAA_Return -> processMenu menu
        otherwise -> return ()

interfaceOptions :: Menu World
interfaceOptions = M.fromList
  [ ("Navigation", (return True , runNavigationW, MAA_Return))
  , ("Trade"     , (dockedW     , runTradeW     , MAA_Return))
  , ("Quit"      , (return True , return MR_Top , MAA_Finish))
  ]

interface = processMenu interfaceOptions

dockedW :: ReaderT World STM Bool
dockedW = getPlayerShipSTM >>= lift . (checkT dockedM) >>= return . isJust

navigationOptions :: Menu NavContext
navigationOptions = M.fromList
  [ ("Display coordinates", (return True  , navDisplay   , MAA_Return))
  , ("Dock"               , (stationNearby, navDock      , MAA_Finish))
  , ("Undock"             , (docked       , navUndock    , MAA_Finish))
  , ("Set course"         , (undocked     , navSetCourse , MAA_Return))
  , ("Engage autopilot"   , (canAutopilot , navTravel    , MAA_Finish))
  ]

navigation = processMenu navigationOptions

docked = ask >>= return . isJust . nc_dockedTo 
undocked = docked >>= return . not

navDisplay :: ReaderT NavContext IO (Menu_Result)
navDisplay = do
  w <- ask
  tstm <- stmRtoIoR stationNearbyM
  lift (atomically $ checkT ship_navModule $ nc_playerShip w) >>= liftIO . print
  if isJust tstm 
    then liftIO (readTVarIO $ fromJust tstm) >>= \st ->
      (liftIO $ putStrLn $ "You're near a station: " ++ station_name st ++ "\n") 
      >> return MR_Stay
    else return MR_Stay

navDock :: ReaderT NavContext IO (Menu_Result)
navDock = do
  tsh <- ask >>= return . nc_playerShip
  tstm <- stmRtoIoR stationNearbyM
  let tst = fromJust tstm
  liftIO $ atomically (startDockingTo tsh tst)
  return MR_Pop

navUndock :: ReaderT NavContext IO (Menu_Result)
navUndock = do
  tsh <- ask >>= return . nc_playerShip
  liftIO $ atomically (startUndocking tsh)
  return MR_Pop
  
stationNearbyM :: ReaderT NavContext STM (Maybe (TVar Station))
stationNearbyM = do
  nc <- ask
  let fn sh = (inHyper sh) || (dockedP sh)
  lift (checkT fn (nc_playerShip nc)) >>= \b ->
    if b
      then return Nothing
      else do
        let tsts = nc_allStations nc
        let tsh = nc_playerShip nc 
        sh <- lift $ readTVar tsh
        closeStations <- lift $ filterM (\tst -> readTVar tst >>= \st -> return $ spaceDistance sh st < 1) tsts
        if null closeStations
          then return Nothing
          else return $ Just $ head closeStations

stationNearby :: ReaderT NavContext STM Bool
stationNearby = stationNearbyM >>= return . isJust

canAutopilot :: ReaderT NavContext STM Bool
canAutopilot = do
  tsh <- ask >>= return . nc_playerShip
  programEmpty <- lift $ checkT navProgramEmpty tsh
  shipIdle <- lift $ checkT isIdle tsh
  return $ not (programEmpty && shipIdle)

runNavigationW :: ReaderT World IO (Menu_Result)
runNavigationW = genNavContext >>= lift . (runReaderT navigation) >> return MR_Top

genNavContext :: ReaderT World IO NavContext
genNavContext = do
  w <- ask
  tsts <- liftIO $ readTVarIO (world_stations w) >>= return . vals
  tsh  <- getPlayerShipIO
  docked <- liftIO $ readTVarIO tsh >>= return . dockedM
  let jgs   = world_jumpgates w
  let plock = world_pauseLock w
  let wtime = world_time w
  return (NavContext tsh tsts docked jgs plock wtime)

navSetCourse :: ReaderT NavContext IO (Menu_Result)
navSetCourse = do
  navContext <- ask
  let tsh = nc_playerShip navContext
  rsts <- stmRtoIoR reachableStations
  liftIO $ showNumberedStationList rsts
  mtst <- getDestination
  if isJust mtst 
    then do 
      stName <- liftIO (readTVarIO (fromJust mtst) >>= return . station_name)
      let tst = fromJust mtst
      liftIO $ putStrLn $ "You've successfully chosen a destination. It would appear " ++
                          "you're heading towards " ++ stName ++ " now."
      shpos <- liftIO $ atomically $ checkT spacePosition tsh
      stmRtoIoR (stationRoutePlanner shpos tst) >>= lift . atomically . (setNavProgram tsh)
      return MR_Pop
    else do
      liftIO $ print "Nothing really happened."
      return MR_Stay

-- navTravelW :: World -> MVar () -> IO ()
-- navTravelW w plock = runReaderT navTravel w

navTravel :: ReaderT NavContext IO (Menu_Result)
navTravel = 
  let fn a = isIdle a && navProgramEmpty a
  in ask >>= lift . atomically . (checkT fn) . nc_playerShip >>= \b ->
    if b
      then return MR_Pop
      else do
        nc <- ask
        let wtime = nc_worldTime nc
        lift $ setCursorPosition 0 0 >> clearFromCursorToScreenEnd
        lift $ putStrLn "\nTravel engaged. Press any key to stop time. "
        slock <- lift $ newTVarIO False
        plock <- ask >>= return . nc_pauseLock
        lift $ unpause plock
        lift $ forkIO $ navTravelRedraw slock wtime nc
        lift $ getChar >> cursorBackward 1
        lift $ atomically $ writeTVar slock True
        lift $ pause plock
        return MR_Top

navTravelRedraw :: TVar Bool -> TVar Int -> NavContext -> IO ()
navTravelRedraw slock wtime nc = do
  setCursorPosition 2 0 >> clearFromCursorToScreenEnd >> setCursorPosition 3 0
  let tsh = nc_playerShip nc
  runReaderT navDisplay nc
  readTVarIO wtime >>= \t -> putStrLn $ "Ticks AD: " ++ show t
  sleep (fromIntegral tickReal)
  programEmpty <- atomically $ checkT navProgramEmpty tsh
  shipIdle <- atomically $ checkT isIdle tsh
  if programEmpty && shipIdle
    then putStrLn "\nThe ship has stopped. Press any key to continue." 
         >> atomically (writeTVar slock True)
    else return ()
  readTVarIO slock >>= \stop ->
    if stop then return ()
            else navTravelRedraw slock wtime nc

stationNames :: [TVar Station] -> STM [String]
stationNames tsts = do
  sts <- mapM readTVar tsts
  let names = P.map station_name sts
  return names

showNumberedList :: [String] -> IO ()
showNumberedList names = do
  let numbers  = P.map (\i -> show i ++ ". ") [1..(length names)]
  let stations = zipWith (++) numbers names
  let line = concatWith "\n" stations
  putStrLn line

showNumberedStationList :: [TVar Station] -> IO ()
showNumberedStationList tsts = do
  names <- atomically $ stationNames tsts
  showNumberedList names

reachableStations :: ReaderT NavContext STM [TVar Station]
reachableStations = ask >>= return . nc_allStations
  --shouldn't mark ALL stations as reachable FIXME

formParser :: [TVar Station] -> String -> Maybe (TVar Station)
formParser = getParsedByNum

getDestination :: ReaderT NavContext IO (Maybe (TVar Station))
getDestination = do
  sts <- nc_allStations <$> ask
  input <- liftIO getLine
  return $ formParser sts input

discoverJumpgate :: Jumpgate -> TVar Player -> STM ()
discoverJumpgate jg tp = readTVar tp >>= \p ->
  let jgs = player_knownJumpgates p
  in  if jg `elem` jgs
        then return ()
        else writeTVar tp p{ player_knownJumpgates = jg:jgs }

type TradeContext = (TVar Owner, TVar Ship, TVar Station)

runTradeW :: ReaderT World IO (Menu_Result)
runTradeW = do
  w <- ask
  to  <- getPlayerIO
  tsh <- getPlayerShipIO 
  tst <- liftIO $ readTVarIO tsh >>= return . dockedSt
  lift $ runReaderT trade (to, tsh, tst)
  return MR_Pop

runTrade :: TVar Owner -> TVar Ship -> TVar Station -> IO ()
runTrade to ts tst = runReaderT trade (to, ts, tst)

trade :: ReaderT TradeContext IO ()
trade = do
  action <- liftIO getLine
  case parseAnyOf allTradeActions action of
    (Just (Buy  (w,a) )) -> do
      buy  w a
      liftIO $ putStrLn $ "You buy " ++ show a ++ " of " ++ show w
    (Just (Sell (w,a) )) -> do
      sell w a
      liftIO $ putStrLn $ "You sell " ++ show a ++ " of " ++ show w
    Nothing -> trade
  if action == "quit"
    then return ()
    else trade

canBuy :: Ware -> Amount -> ReaderT TradeContext IO Bool
canBuy bw ba = do
  (to, tsh, tst) <- ask
  liftIO $ do
    sh <- readTVarIO tsh
    st <- readTVarIO tst
    o <- readTVarIO to
    let enoughSpaceP = ship_freeSpace sh >= weight bw * fromIntegral ba
    let enoughMoneyP = owner_money o >= stockSellTotal st bw ba
    let enoughWareP = enoughWarePure bw ba st
    return $ and [enoughSpaceP, enoughMoneyP, enoughWareP] 

buy :: Ware -> Amount -> ReaderT TradeContext IO ()
buy bw ba = do
  (to, tsh, tst) <- ask
  st <- liftIO $ readTVarIO tst
  cb <- canBuy bw ba

  liftIO $
    if cb then atomically $ do
            removeWare bw ba tst
            addWare bw ba tsh
            removeMoney (stockSellTotal st bw ba) to
            addMoney (stockSellTotal st bw ba) tst
          else return ()

canSell :: Ware -> Amount -> ReaderT TradeContext IO Bool
canSell sw sa = do
  (to, tsh, tst) <- ask
  liftIO $ do
    o <- readTVarIO to
    sh <- readTVarIO tsh
    st <- readTVarIO tst
    let enoughMoneyP = station_money st >= stockBuyTotal st sw sa
    let enoughWareP = enoughWarePure sw sa sh

    return $ and [enoughMoneyP, enoughWareP] 

sell :: Ware -> Amount -> ReaderT TradeContext IO ()
sell sw sa = do
  (to, tsh, tst) <- ask
  st <- liftIO $ readTVarIO tst
  cs <- canSell sw sa

  liftIO $
    if cs then atomically $ do
            removeWare sw sa tsh
            addWare sw sa tst
            removeMoney (stockBuyTotal st sw sa) tst
            addMoney (stockBuyTotal st sw sa) to
          else return ()
