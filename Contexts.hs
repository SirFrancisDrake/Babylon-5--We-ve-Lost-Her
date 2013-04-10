
module Contexts where

import Control.Concurrent
import Control.Concurrent.STM
import Data.IntMap

import DataTypes
import Jumpgates

data NavContext = NavContext
  { nc_playerShip  :: TVar Ship
  , nc_allStations :: [TVar Station]
  , nc_dockedTo    :: Maybe (TVar Station)
  , nc_jumpgates   :: TVar (IntMap Jumpgate)
  , nc_pauseLock   :: MVar ()
  , nc_worldTime   :: TVar Int
  }

