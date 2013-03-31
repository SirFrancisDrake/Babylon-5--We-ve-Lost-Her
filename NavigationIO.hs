
module NavigationIO where

import Control.Concurrent.STM

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
