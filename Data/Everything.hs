
module Data.Everything where

import Data.Owners
import Data.Ships
import DataTypes

data StoredRace = StoredRace
  { sr_owner :: Owner
  , sr_ships :: [Ship]
  , sr_stations :: [Station]
  }
  deriving ()

startingRaces :: [StoredRace]
startingRaces =
  [ minbariS
  ]

minbariS :: StoredRace
minbariS = StoredRace minbariOwner minbariShips [] -- tmp placeholder FIXME
