module ShipStats where

data ShipStats = ShipStats { shipStats_maxAccel :: Int
                           , shipStats_cargoHold :: Int
                           , shipStats_battery :: Int
                           , shipStats_scannerRange :: Double
                           }
            deriving (Show)

defaultStats = ShipStats 0 1 2 200.0

shipStats _ = defaultStats
