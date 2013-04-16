
module Contexts where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.IntMap

import Auxiliary.IntMap (vals)
import Auxiliary.Transactions
import Data.Encounters
import DataTypes
import ErrorMessages
import Encounters
import Interface.Base
import Jumpgates
import Ships

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
  , nc_encounters  :: [Encounter]
  , nc_travelContinuation :: TVar (ReaderT NavContext IO ())
  }

genNavContext :: TVar Ship -> ReaderT World STM NavContext
genNavContext tsh = do
  w <- ask
  tsts <- lift (readTVar (world_stations w)) >>= return . vals
  docked <- lift (readTVar tsh) >>= return . dockedM
  let jgs   = world_jumpgates w
  let plock = world_pauseLock w
  let wtime = world_time w
  return (NavContext tsh tsts docked jgs plock wtime err_nc_noEncounters err_nc_noCont)

genNavContextR :: ReaderT World STM NavContext
genNavContextR = do
  nc <- getPlayerShipSTM >>= genNavContext
  fecs <- filterM (\e -> encounter_check e) encounters
  tcont <- lift $ newTVar (err_nc_noCont)
  return nc{ nc_encounters = fecs 
           , nc_travelContinuation = tcont }

type TradeContext = (TVar Owner, TVar Ship, TVar Station)

genTradeContext :: TVar Owner -> TVar Ship -> TVar Station -> TradeContext
genTradeContext to tsh tst = (to, tsh, tst)

