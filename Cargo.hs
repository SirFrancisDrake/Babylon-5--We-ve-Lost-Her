module Cargo where

type Cargo = [(Ware, Price)]
type Price = Int

data Ware = Gold | Lumber | Oil
    deriving (Eq, Show)

defaultCargo = []
