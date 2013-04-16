
module Contexts where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.IntMap

import Auxiliary.IntMap (vals)
import Auxiliary.Transactions
import DataTypes
import ErrorMessages
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
getPlayerSTM :: ReaderT World STM Player
getPlayerSTM = ask >>= lift . readTVar . world_player 

getPlayerShipSTM :: ReaderT World STM (TVar Ship)
getPlayerShipSTM = ask >>= lift . readTVar . world_player >>= return . player_selectedShip

getPlayerOwnerSTM :: ReaderT World STM (TVar Owner)
getPlayerOwnerSTM = ask >>= lift . readTVar . world_player >>= return . player_owner

getPlayerIO :: ReaderT World IO Player
getPlayerIO = stmRtoIoR getPlayerSTM

getPlayerShipIO :: ReaderT World IO (TVar Ship)
getPlayerShipIO = stmRtoIoR getPlayerShipSTM

getPlayerOwnerIO :: ReaderT World IO (TVar Owner)
getPlayerOwnerIO = stmRtoIoR getPlayerOwnerSTM

data NavContext = NavContext
  { nc_playerShip  :: TVar Ship
  , nc_allStations :: [TVar Station]
  , nc_dockedTo    :: Maybe (TVar Station)
  , nc_jumpgates   :: TVar (IntMap Jumpgate)
  , nc_pauseLock   :: MVar ()
  , nc_worldTime   :: TVar Int
  , nc_encounters  :: [Encounter]
  , nc_travelContinuation :: TVar (ReaderT NavContext IO ())
  , nc_questVariables :: TVar QuestVars
  }

genNavContext :: TVar Ship -> ReaderT World STM NavContext
genNavContext tsh = do
  w <- ask
  tsts <- lift (readTVar (world_stations w)) >>= return . vals
  docked <- lift (readTVar tsh) >>= return . dockedM
  qvars <- lift (readTVar $ world_player w) >>= return . player_questVars
  let jgs   = world_jumpgates w
  let plock = world_pauseLock w
  let wtime = world_time w
  return (NavContext tsh tsts docked jgs 
            plock wtime err_nc_noEncounters err_nc_noCont
            qvars)

genNavContextR :: ReaderT World STM NavContext
genNavContextR = do
  nc <- getPlayerShipSTM >>= genNavContext
  fecs <- ask >>= (filterM (\e -> encounter_check e)) . world_encounters
  tcont <- lift $ newTVar (err_nc_noCont)
  return nc{ nc_encounters = fecs 
           , nc_travelContinuation = tcont }

type TradeContext = (TVar Owner, TVar Ship, TVar Station)

genTradeContext :: TVar Owner -> TVar Ship -> TVar Station -> TradeContext
genTradeContext to tsh tst = (to, tsh, tst)

