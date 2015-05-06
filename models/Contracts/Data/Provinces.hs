module Data.Provinces
( startingProvinces
) where

import Data.Function (on)
import Data.List ((\\))

import Auxiliary.Graph
import DataTypes

startingProvinces =
  foldl (\acc (a,b) -> acc ++ ([a,b] \\ acc)) [] adjacencyTable

adjacencyTable :: [ (Province, Province) ]
adjacencyTable =
  [ (cal, rom)
  , (rom, cam)
  , (bru, cam)
  , (tar, bru)
  , (mas, rom)
  ]

cal = Province "Calabria"   (0,0) [] []
rom = Province "Roma"       (0,0) [] []
cam = Province "Campagnia"  (0,0) [] []
bru = Province "Brundisium" (0,0) [] []
tar = Province "Tarentum"   (0,0) [] []
mas = Province "Massalia"   (0,0) [] []

instance Eq Province where
  (==) = on (==) province_name

instance Graph Province where
  adjacentNodes p = 
    let fn acc (a,b)
          | a == p = acc ++ [b]
          | b == p = acc ++ [a]
          | otherwise = acc
    in  foldl fn [] adjacencyTable
