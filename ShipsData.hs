module ShipsData where

import AI
import DataTypes
import Navigation
import ShipsAndStations
import ShipStats
import Vector
import Wares
import Wrappers

--TMP FIXME
spaceN :: Double -> Double -> Double -> NavModule
spaceN x y z = NavModule
                (SNPSpace (Space (Vector3D x y z) Normalspace))
                Idle

defaultNavModule = spaceN 0.01 0.01 0.01

shipsDataDefShip = Ship "empty name"
                        (error "ShipsData.hs: undefined") 
                        (error "ShipsData.hs: undefined") 
                        defaultNavModule
                        defaultAI 
                        defaultCargo 
                        (error "ShipsData.hs: undefined")

shStandardRhino :: Ship
shStandardRhino = let thisShipStats = shipStats Rhino
                  in shipsDataDefShip{ ship_class = Rhino
                                     , ship_stats = thisShipStats
                                     }

shStandardLiandra :: Ship
shStandardLiandra = let thisShipStats = shipStats Liandra
                  in shipsDataDefShip{ ship_class = Liandra
                                     , ship_stats = thisShipStats
                                     }
                        
shStandardClark :: Ship
shStandardClark = let thisShipStats = shipStats Clark
                  in shipsDataDefShip{ ship_class = Clark
                                     , ship_stats = thisShipStats
                                     }

shStandardLondo :: Ship
shStandardLondo = let thisShipStats = shipStats Londo
                  in shipsDataDefShip{ ship_class = Londo
                                     , ship_stats = thisShipStats
                                     }
                        
shStandardGQuan :: Ship
shStandardGQuan = let thisShipStats = shipStats GQuan
                  in shipsDataDefShip{ ship_class = GQuan
                                     , ship_stats = thisShipStats
                                     }
                        
shStandardHel :: Ship
shStandardHel = let thisShipStats = shipStats Hel
                  in shipsDataDefShip{ ship_class = Hel
                                     , ship_stats = thisShipStats
                                     }
