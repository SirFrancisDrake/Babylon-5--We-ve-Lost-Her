module ShipsData where

import AI
import DataTypes
import Navigation
import ShipsAndStations
import ShipStats
import Wares
import Wrappers

shipsDataDefShip = Ship "empty name"
                        undefined 
                        undefined 
                        undefined 
                        defaultAI 
                        defaultCargo 
                        undefined

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
