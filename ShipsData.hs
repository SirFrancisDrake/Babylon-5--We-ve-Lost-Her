module ShipsData where

import Navigation
import Owners
import Ships
import Wares
import Wrappers

shStandardRhino :: Ship
shStandardRhino = let thisShipStats = shipStats Rhino
                  in Ship   "empty name"
                            Rhino 
                            thisShipStats 
                            defaultNavModule 
                            defaultCargo 
                            defaultOwner
                            defaultAI

shStandardLiandra :: Ship
shStandardLiandra = let thisShipStats = shipStats Clark
                  in Ship   "empty name"
                            Liandra 
                            thisShipStats 
                            defaultNavModule 
                            defaultCargo 
                            defaultOwner
                            defaultAI
                        
shStandardClark :: Ship
shStandardClark = let thisShipStats = shipStats Clark
                  in Ship   "empty name"
                            Clark 
                            thisShipStats 
                            defaultNavModule 
                            defaultCargo 
                            defaultOwner
                            defaultAI

shStandardLondo :: Ship
shStandardLondo = let thisShipStats = shipStats Londo
                  in Ship   "empty name"
                            Londo 
                            thisShipStats 
                            defaultNavModule 
                            defaultCargo 
                            defaultOwner
                            defaultAI
                        
shStandardGQuan :: Ship
shStandardGQuan = let thisShipStats = shipStats GQuan
                  in Ship   "empty name"
                            GQuan 
                            thisShipStats 
                            defaultNavModule 
                            defaultCargo 
                            defaultOwner
                            defaultAI
                        
shStandardHel :: Ship
shStandardHel = let thisShipStats = shipStats Hel
                  in Ship   "empty name"
                            Hel 
                            thisShipStats 
                            defaultNavModule 
                            defaultCargo 
                            defaultOwner
                            defaultAI
