module GameCycle where

import Control.Concurrent.STM
import Control.Monad.Reader
import Data.IntMap

import Owners
import Ships
import Stations
import System.Posix.Unistd (sleep)
import Wrappers

newtype MutableMap a = TVar (IntMap (TVar a))

data World = World
    { world_stations :: MutableMap Station
    , world_ships :: MutableMap Ship
    , world_owners :: MutableMap Owner
    } deriving ()

threadDelay :: Int
threadDelay = 3 -- seconds

cycle :: World -> IO ()
cycle world = do
    runReaderT (updateStations >> updateShips >> updatePlayers) world
    sleep threadDelay
    cycle world

instance Show World where
    show _ = "The birds have vanished in the sky, and now the last cloud drains away. \nWe sit together, the mountain and I until only the mountain remains."
