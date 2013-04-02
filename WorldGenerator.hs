
module WorldGenerator
( generateWorld
) where

import Control.Concurrent.STM
import qualified Data.IntMap as I
import Data.List (foldr)
import Prelude hiding (map)
import qualified Prelude as P (map)

import Auxiliary.IntMap
import Data.Everything
import DataTypes
import Jumpgates
import JumpgatesData

generateWorld :: STM World
generateWorld = generateWorldFrom startingRaces

generateWorldFrom :: [StoredRace] -> STM World
generateWorldFrom srs = do
    tsrs <- mapM makeRace srs
    let appendTriples (a1, a2, a3) (b1, b2, b3) = (a1 ++ b1, a2 ++ b2, a3 ++ b3)
    let (towners, tships, tstations) = foldr appendTriples ([],[],[]) tsrs
    let i l = I.fromList $ zip [0..] l
    itst <- newTVar (i tstations)
    itsh <- newTVar (i tships)
    iown <- newTVar (i towners)
    ijg  <- newTVar (i jg_startingJumpgates)
    return (World itst itsh iown ijg)

makeRace :: StoredRace -> STM ([TVar Owner], [TVar Ship], [TVar Station])
makeRace (StoredRace o shs sts) = do
    to <- newTVar o
    tshs <- mapM (\s -> newTVar s{ ship_owner = to } ) shs
    tsts <- mapM (\s -> newTVar s{ station_owner = to } ) sts
    return ([to], tshs, tsts)
    
