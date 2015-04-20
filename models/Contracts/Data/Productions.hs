
module Data.Productions
( startingProductions
)

where

import Data.StoringTypes
import DataTypes

startingProductions = [ co1, ti1, br1, sw1, sw2]

co1 = ("Campagnia", Production undefined [] [(Copper, 30)] 1 [])
ti1 = ("Roma", Production undefined [] [(Tin, 10)] 1 [])
br1 = ("Calabria", 
        Production 
          undefined 
          [(Tin, 10), (Copper, 30)] 
          [(Bronze, 5)] 
          1 
          [])
sw1 = ("Calabria", 
        Production 
          undefined 
          [(Bronze, 5)] 
          [(Sword, 1)] 
          1 
          [])
sw2 = ("Calabria", 
        Production 
          undefined 
          [(Sword, 1)] 
          []
          1 
          [])
