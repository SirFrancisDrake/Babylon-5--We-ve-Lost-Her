module Data.Stations where

import Data.IntMap

import Currency
import DataTypes
import Navigation
import ShipsAndStations
import Stock
import qualified Vector as V
import Wares
import Wrappers

minbariStations = [ minbariPrime
                  ]

triOptimumStations = [ citadelStation
                     ]

gibarianStations = [ solarisOne
                   ]

citadelStation :: Station
citadelStation = Station "Citadel station" 
                         (Space (V.fromList [0,0,0]) Normalspace)
                         (makeStock [ (Fuel,10,0,0,30)
                                    , (Books,20,0,0,15)
                                    , (CyberModules,300,0,0,270)
                                    , (Silicium,1,0,0,400)
                                    ])
                         defaultMoney 
                         [] 
                         undefined -- TriOptimum Corp.
                         "The first stellar station to be operated by artificial intelligence. The AI named Shodan maintains most of the automated systems and onboard droids. This is the center of cutting edge research in modern weaponry, computers and molecular biology."
                         []

solarisOne :: Station
solarisOne = Station "Solaris research station" 
                     (Space (V.fromList [120,300,451]) Normalspace)
                     defaultStock 
                     defaultMoney 
                     [] 
                     undefined -- Gibarian Inc.
                     "Solaris research station is located in low orbit of planet Solaris, believed to be the only known planetary-sized alien life form. This is the center of solaris-related research and observations."
                     []

minbariPrime :: Station
minbariPrime = Station "Minbari Prime" 
                     (Space (V.fromList [320,100,23]) Normalspace)
                     defaultStock 
                     defaultMoney 
                     [] 
                     undefined -- Minbari Republic
                     "This is Minbari Prime, home planet of the Minbari. I do not dare say more."
                     []