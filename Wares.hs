module Wares where

import Wrappers

data Ware = Fuel | Food | Supplies
    deriving (Eq, Show, Ord)

type Cargo = [(Ware, Amount)]
