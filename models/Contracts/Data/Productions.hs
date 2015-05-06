
module Data.Productions
( startingProductions
)

where

import Data.StoringTypes
import DataTypes

startingProductions = [ co1, ti1, br1, sw1]

co1 = ("Campagnia", "SPQR", Production undefined undefined [] [(Copper, 30)] 1 [])
ti1 = ("Roma", "SPQR", Production undefined undefined [] [(Tin, 10)] 1 [])
br1 = ("Calabria", "SPQR", 
        Production 
          undefined 
          undefined 
          [(Tin, 10), (Copper, 30)] 
          [(Bronze, 5)] 
          1 
          [])
sw1 = ("Calabria", "SPQR", 
        Production 
          undefined 
          undefined 
          [(Bronze, 5)] 
          [(Sword, 1)] 
          1 
          [])
sw2 = ("Calabria", "SPQR", 
        Production 
          undefined 
          undefined 
          [(Sword, 1)] 
          []
          1 
          [])
