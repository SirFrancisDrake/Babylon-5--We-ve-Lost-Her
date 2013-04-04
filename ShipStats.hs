module ShipStats where

data ShipStats = ShipStats 
  { shipStats_maxAccel :: Int
  , shipStats_topSpeed :: Double -- FIXME 
  , shipStats_cargoHold :: Int
  , shipStats_battery :: Int
  , shipStats_scannerRange :: Double
  }
  deriving (Show)

defaultStats = ShipStats 0 3 1 2 200

shipStats _ = defaultStats
