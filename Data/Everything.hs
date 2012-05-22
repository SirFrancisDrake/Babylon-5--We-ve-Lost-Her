
module Data.Everything where

import Data.Owners
import Data.Ships
import Data.Stations
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
  , gibarianS
  , triOptimumS
  ]

minbariS :: StoredRace
minbariS = StoredRace minbariOwner minbariShips minbariStations -- tmp placeholder FIXME

triOptimumS = StoredRace triOptimumOwner [] triOptimumStations -- tmp placeholder FIXME

gibarianS = StoredRace gibarianOwner [] gibarianStations -- tmp placeholder FIXME
