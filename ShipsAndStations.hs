
{-# LANGUAGE FlexibleInstances #-}

module ShipsAndStations where

-- Merged those on March 29th, because it's the only way to
-- let ships contain pointers to stations and likewise

import Control.Concurrent
import Control.Concurrent.STM
import Data.Function (on)
import Data.IntMap hiding (fromList)
import qualified Data.IntMap as I (fromList)
import Prelude hiding (length)

import AI
import Currency
import DataTypes
import ErrorMessages
import InterfaceShow
import IntMapAux
import GlobalConst (tickGame)
import Navigation
import ShipStats
import Stock
import Vector hiding (fromList)
import qualified Vector as V (fromList)
import Wares
import Wrappers

-- SHIPS.HS

-- type Ships = IntMap (TVar Ship)
-- DEPRECATED 12 apr 2012


-- STATIONS.HS

-- type Stations = IntMap (TVar Station)
-- DEPRECATED 12 apr 2012

