
module Contexts where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.IntMap

import Auxiliary.IntMap (vals)
import Auxiliary.Transactions
import DataTypes
import Jumpgates
import Ships

getPlayerShipSTM :: ReaderT World STM (TVar Ship)
getPlayerShipSTM = ask >>= lift . readTVar . world_player >>= return . player_selectedShip

getPlayerSTM :: ReaderT World STM (TVar Owner)
getPlayerSTM = ask >>= lift . readTVar . world_player >>= return . player_owner

getPlayerShipIO :: ReaderT World IO (TVar Ship)
getPlayerShipIO = stmRtoIoR getPlayerShipSTM

getPlayerIO :: ReaderT World IO (TVar Owner)
getPlayerIO = stmRtoIoR getPlayerSTM

data NavContext = NavContext
  { nc_playerShip  :: TVar Ship
  , nc_allStations :: [TVar Station]
  , nc_dockedTo    :: Maybe (TVar Station)
  , nc_jumpgates   :: TVar (IntMap Jumpgate)
  , nc_pauseLock   :: MVar ()
  , nc_worldTime   :: TVar Int
  }

genNavContext :: TVar Ship -> ReaderT World STM NavContext
genNavContext tsh = do
  w <- ask
  tsts <- lift (readTVar (world_stations w)) >>= return . vals
  docked <- lift (readTVar tsh) >>= return . dockedM
  let jgs   = world_jumpgates w
  let plock = world_pauseLock w
  let wtime = world_time w
  return (NavContext tsh tsts docked jgs plock wtime)

genNavContextR :: ReaderT World STM NavContext
genNavContextR = do
  w <- ask
  tsts <- lift (readTVar (world_stations w)) >>= return . vals
  tsh  <- getPlayerShipSTM
  docked <- lift (readTVar tsh) >>= return . dockedM
  let jgs   = world_jumpgates w
  let plock = world_pauseLock w
  let wtime = world_time w
  return (NavContext tsh tsts docked jgs plock wtime)

type TradeContext = (TVar Owner, TVar Ship, TVar Station)

genTradeContext :: TVar Owner -> TVar Ship -> TVar Station -> TradeContext
genTradeContext to tsh tst = (to, tsh, tst)

