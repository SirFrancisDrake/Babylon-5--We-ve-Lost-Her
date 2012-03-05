module StationsData where

import Data.IntMap

import Currency
import Navigation
import Stations
import Stock
import qualified Vector as V
import Wares
import Wrappers

defaultStations :: IntMap Station
defaultStations = fromList $ zip [0..] [citadelStation, solarisOne]

citadelStation :: Station
citadelStation = Station "Citadel station" 
                         (Space (V.fromList [0,0,0]) Normalspace)
                         defaultStock 
                         defaultMoney 
                         [] 
                         "TriOptimum Corp."
                         []

solarisOne :: Station
solarisOne = Station "Solaris research station" 
                     (Space (V.fromList [120,300,451]) Normalspace)
                     defaultStock 
                     defaultMoney 
                     [] 
                     "Gibarian Inc."
                     []
