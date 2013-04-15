module ErrorMessages where

-- Contexts.hs
err_noEncounters = error "Encounters undefined."

-- Navigation.hs
err_distanceNormalHyper = error "Can't measure distance between normalspace and hyperspace."

-- Interface.hs
err_recognizeGoArguments = error "Interface: Can't recognize go arguments."
err_recognizeTradeArguments = error "Interface: Can't recognize trade arguments."
err_argumentsShouldBeEmpty = error "Interface: Arguments for that command should be empty."

-- ShipsAndStations.hs
err_dockedStNotExist = error "Trying to access the station I'm docked to, but I'm not docked"
err_dockingStNotExist = error "Trying to access the station I'm docking to, but I'm not docking"

-- Space.hs
err_cantJumpNormalToNormal = error "Space: Can't jump from normalspace to normalspace."
err_cantJumpHyperToHyper = error "Space: Can't jump from hyperspace to hyperspace."
