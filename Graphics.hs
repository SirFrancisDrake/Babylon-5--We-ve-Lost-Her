module Graphics where

import Ships
import ShipsAndStations
import Wares

class Render a where
    render :: a -> String

instance Render Station where
    render st =
        let name = station_name st
            cargo = station_cargo st
            ships = station_dockingBay st
            owner = station_owner st
        in "This is " ++ name ++ " owned by " ++ owner ++
           "\nStation stocks include: " ++ show cargo ++ 
           "\nShips that are currently in the docking bay: " 
           ++ show ships
