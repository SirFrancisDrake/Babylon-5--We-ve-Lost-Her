module Space 
  ( toNormal
  , toHyper
  )
  where

import ErrorMessages
import Navigation
import Vector

space_normalHyperCoeff = 100 -- ratio of distance conservation through hyperspace
cf = space_normalHyperCoeff -- just a small alias

toNormal :: NavPosition -> NavPosition
toNormal (Space v Hyperspace) = Space (v * fromIntegral cf) Normalspace
toNormal (Space _ Normalspace) = err_cantJumpNormalToNormal

toHyper :: NavPosition -> NavPosition
toHyper (Space v Normalspace) = Space (v * fromIntegral cf) Hyperspace
toHyper (Space _ Hyperspace) = err_cantJumpHyperToHyper

-- Generalization note: toN and toH should be continuous mappings from R^3 to R^3
-- such that toN (toH n) = n and toH (toN h) = h
-- Moreover, for each normalspace v_1, v_2 the following property should hold:
-- d_n( v_1,v_2 ) >= d_h( toH( v_1 ), toH( v_2 ) )
-- where d_n and d_h are distances in hyperspace and normalspace respectively
-- Given that, particular mapping could be anything.
