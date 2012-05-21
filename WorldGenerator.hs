
module WorldGenerator
( makeNewWorld
) where

import Control.Concurrent.STM

import Data.Everything
import DataTypes
import World

makeNewWorld :: [StoredRace] -> STM World
makeNewWorld srs = do
    tsrs <- mapM makeRace srs
    let appendTriples (a1, a2, a3) (b1, b2, b3) = (a1 ++ b1, a2 ++ b2, a3 ++ b3)
    let (towners, tships, tstations) = fold appendTriples ([],[],[]) tsrs
    return (World tstations, tships, towners)

makeRace :: StoredRace -> STM ([TVar Owner], [TVar Ship], [TVar Station])
makeRace (StoredRace o shs sts) = do
    to <- newTVar o
    tshs <- mapM (\f -> newTVar $ f to) shs
    tsts <- mapM (\f -> newTVar $ f to) sts
    return ([to], tshs, tsts)
    
