
module Data.Owners where

import DataTypes

-- Alliance Races

humansOwner = Owner
            "Earth Alliance"
            []
            []
            (Nation (-1))
            8999

minbariOwner = Owner
            "Minbari Republic"
            []
            []
            (Nation 9001)
            9002

narnOwner = Owner
            "Narn Regime"
            []
            []
            (Nation 0)
            11

centauriOwner = Owner
            "Centauri Republic"
            []
            []
            (Nation 1)
            7


-- League of Non-Allied Worlds

-- Other races
triOptimumOwner = Owner
            "TriOptimum Corp."
            []
            []
            (Nation 0)
            7

gibarianOwner = Owner
            "Gibarian Inc."
            []
            []
            (Nation 1)
            8
