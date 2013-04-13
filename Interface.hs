
{-# LANGUAGE FlexibleInstances #-}

module Interface 
( TradeAction(..)
, allTradeActions
, interface
) 
where

import Control.Applicative hiding ((<|>))
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad.Reader
import qualified Data.Map as M
import Data.Maybe (isJust, fromJust)
import Prelude hiding (filter, map)
import qualified Prelude as P (filter, map)
import System.Console.ANSI
import System.Posix (sleep)
import Text.ParserCombinators.Parsec (Parser(..), parse)

import Auxiliary.Concurrent
import Auxiliary.Parsec
import Auxiliary.Transactions
import Auxiliary.StringFunctions
import Contexts
import DataTypes
import GlobalConst
import Interface.Actions
import Interface.Parsers
import Jumpgates
import NavigationIO
import Parsable
import Ships
import Space
import TradeIO
import Wares
import Wrappers

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
runNavigationW = stmRtoIoR genNavContextR >>= lift . (runReaderT navigation) >> return MR_Top

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
      shpos <- liftIO $ atomically $ checkT (navModule_position . ship_navModule)  tsh
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

runTradeW :: ReaderT World IO (Menu_Result)
runTradeW = do
  w <- ask
  to  <- getPlayerIO
  tsh <- getPlayerShipIO 
  tst <- liftIO $ readTVarIO tsh >>= return . dockedSt
  lift $ runReaderT trade (genTradeContext to tsh tst)
  return MR_Pop

runTrade :: TVar Owner -> TVar Ship -> TVar Station -> IO ()
runTrade to ts tst = runReaderT trade (to, ts, tst)

trade :: ReaderT TradeContext IO ()
trade = do
  action <- liftIO getLine
  case parseAnyOf allTradeActions action of
    (Just (Buy  (w,a) )) -> do
      stmRtoIoR $ buy w a
      liftIO $ putStrLn $ "You buy " ++ show a ++ " of " ++ show w
    (Just (Sell (w,a) )) -> do
      stmRtoIoR $ sell w a
      liftIO $ putStrLn $ "You sell " ++ show a ++ " of " ++ show w
    Nothing -> trade
  if action == "quit"
    then return ()
    else trade

askPolitely :: String -> Parser a -> IO a
askPolitely question p = do
  putStrLn question
  let tryOnce = getLine >>= \ans ->
             case parse p "" ans of
               Left err -> putStrLn "Failed to parse your input. Try again" >> tryOnce
               Right a -> return a
  tryOnce

data OptionsAction =
  RenameShip String
  | DoNothing
  deriving (Show)

instance Parsable TradeAction where
  getParser (Buy _)  = parserBuy
  getParser (Sell _) = parserSell

allTradeActions = [Buy undefined, Sell undefined]
