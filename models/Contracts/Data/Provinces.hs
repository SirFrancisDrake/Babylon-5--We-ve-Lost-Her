
module Data.Provinces
( startingProvinces
) where

import DataTypes

startingProvinces = [ cal, rom, cam ]

cal = Province "Calabria"  (0,0) [] []
rom = Province "Roma"      (0,0) [] []
cam = Province "Campagnia" (0,0) [] []
