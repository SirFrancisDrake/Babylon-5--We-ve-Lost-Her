module Data.Ships where

import AI
import DataTypes
import Navigation
import ShipStats
import Vector
import Wares

minbariShips = 
  [ valen'tha
  ]

spaceN :: Double -> Double -> Double -> NavModule
spaceN x y z = 
  NavModule
    (SNPSpace (Space (Vector3D x y z) Normalspace))
    Idle
    []

valen'tha :: Ship
valen'tha = 
  Ship
    "Valen'tha"
    Sharlin
    defaultStats
    (spaceN 30 27 29.2)
    defaultAI
    defaultCargo
    undefined
    undefined -- FIXME

