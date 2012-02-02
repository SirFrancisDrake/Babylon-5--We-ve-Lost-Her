module Transactions where

import Control.Concurrent.STM

-- import Players
import Stations
import Wares
import Wrappers

-- Remove 30 Credits, add 15 Fuel, if not enough Credits, do nothing
-- Try the above variant, if it fails, remove (credits / 5), add fuel respectively
-- Same as 1 and 2, but with multiple wares to add and remove
purchase :: Station -> Player -> Ware -> Amount -> STM ()
purchase stat pla ware amount = 
    let wareTotalCost = wareCost stat ware
        wareAmount = wareAmount stat ware
        playerMoney = money pla
        doIWantThis = (wareAmount >= amount) && (playerMoney >= wareTotalCost)
    in if (not doIWantThis) then return ()
                            else do removeMoney pla wareTotalCost
                                    removeWare ware amount stat
                                    addWare ware amount pla
                                    return ()
