module ShipStats where

data ShipStats = ShipStats { shipStats_maxAccel :: Int
                           , shipStats_cargoHold :: Int
                           , shipStats_battery :: Int
                           }
            deriving (Show)
