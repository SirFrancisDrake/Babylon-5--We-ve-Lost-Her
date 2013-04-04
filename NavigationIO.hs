
module NavigationIO where

import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Function (on)
import qualified Data.IntMap as I
import Data.List (sortBy)

import Auxiliary.Graph
import Data.Jumpgates
import DataTypes
import GlobalConst (const_jg_exit_radius)
import Jumpgates
import Navigation
import Space
import Vector

-- Randomly choose a spot inside a r-circle around p, but not p
-- it's gonna use IO later, hence the type. TODO implement pseudorandom choice
randomizeAround :: NavPosition -> Double -> IO NavPosition
randomizeAround (Space (Vector3D x y z) t) _ = return $
  Space (Vector3D (x + 1/10) (y + 1/10) (z + 1/10)) t

departureAround :: NavPosition -> NavPosition
departureAround (Space (Vector3D x y z) t) =
  Space (Vector3D (x + 1/10) (y + 1/10) (z + 1/10)) t

jump :: NavPosition -> SpaceType -> IO NavPosition
jump entryPoint stype = 
  randomizeAround (toSpaceType stype entryPoint) const_jg_exit_radius

-- STM not required, but it will be when we get to jump-capable ships
getJumpEnginePos :: NavStatus -> SpaceType -> STM NavPosition
getJumpEnginePos (Jumping (JE_Jumpgate jg) _) stype =
  case stype of
    Normalspace -> return (jg_normal jg)
    Hyperspace -> return (jg_hyper jg)

closestJumpgate :: NavPosition -> ReaderT World STM Jumpgate
closestJumpgate p@(Space v t) =
  ask >>= lift . readTVar . world_jumpgates >>= return . head . (sortBy sortFn) . I.elems
  where sortFn = compare `on` (\jg -> distance v (jg_vector jg t))

dumbRoutePlanner :: NavPosition -> NavPosition -> ReaderT World STM NavProgram
dumbRoutePlanner p1@(Space v1 t1) p2@(Space v2 t2)
  | and [t1 == t2, t1 == Hyperspace] = return [NA_MoveTo v2]
  | and [t1 == t2, t1 == Hyperspace] = do
    jg1 <- closestJumpgate p1
    jg2 <- closestJumpgate p2
    return [ NA_MoveTo (jg_vector jg1 Normalspace)
           , NA_Jump Hyperspace
           , NA_MoveTo (jg_vector jg2 Hyperspace)
           , NA_Jump Normalspace
           , NA_MoveTo v2 ]
  | t1 == Hyperspace = closestJumpgate p2 >>= \jg -> 
    return
     [ NA_MoveTo (jg_vector jg Hyperspace)
     , NA_Jump Normalspace
     , NA_MoveTo v2 ]
  | t1 == Normalspace = closestJumpgate p1 >>= \jg -> 
    return
      [ NA_MoveTo (jg_vector jg Normalspace)
      , NA_Jump Hyperspace
      , NA_MoveTo v2 ]

smartRoutePlanner :: NavPosition -> NavPosition -> ReaderT World STM NavProgram
smartRoutePlanner p1@(Space v1 t1) p2@(Space v2 t2)
  | and [t1 == t2, t1 == Normalspace] = do
    jg1 <- closestJumpgate p1
    jg2 <- closestJumpgate p2
    return $ [ NA_MoveTo (jg_vector jg1 Normalspace)
             , NA_Jump Hyperspace ]
             ++ (jgRoutePlanner jg1 jg2) ++
             [ NA_Jump Normalspace
             , NA_MoveTo v2 ]
  | otherwise = error "Traveling in hyperspace is restricted to jumpgates " -- TODO
--  | and [t1 == t2, t1 == Hyperspace] = return [NA_MoveTo v2]
--  | t1 == Hyperspace = closestJumpgate p2 >>= \jg -> 
--    return
--     [ NA_MoveTo (jg_vector jg Hyperspace)
--     , NA_Jump Normalspace
--     , NA_MoveTo v2 ]
--  | t1 == Normalspace = closestJumpgate p1 >>= \jg -> 
--    return
--      [ NA_MoveTo (jg_vector jg Normalspace)
--      , NA_Jump Hyperspace
--      , NA_MoveTo v2 ]

jgRoutePlanner :: Jumpgate -> Jumpgate -> NavProgram
jgRoutePlanner jg1 jg2 =
  let routes = findRoutes jg1 jg2
      sumDistance route =  sum $ map (uncurry jgsDistance) (zip route (tail route))
      shortest =
        if null routes
          then error $ "NavigationIO: jgRoutePlanner: can't find a route " ++
                 "from " ++ show jg1 ++ " to " ++ show jg2
          else head $ sortBy (on compare sumDistance) routes
  in map NA_MoveInHyper shortest
  

tickNavProgram :: NavModule -> ReaderT World STM NavModule
tickNavProgram nm
  | null (navModule_program nm) = return nm
  | otherwise = do
    let (p:ps) = navModule_program nm
    let stat = navModule_status nm
    (ns, np) <- 
      case p of
        (NA_MoveTo v) -> return (MovingToSpace 0 v   , ps)
        (NA_Dock tst) -> return (DockingToStation tst, ps)
        NA_Undock     -> return (Undocking           , ps)
        (NA_Jump st)  -> closestJumpgate (spacePosition $ navModule_position nm) >>=
          \jg -> return (Jumping (JE_Jumpgate jg) st , ps)
    case stat of
      Idle -> return nm{ navModule_status = ns
                       , navModule_program = np }
      otherwise -> return nm
