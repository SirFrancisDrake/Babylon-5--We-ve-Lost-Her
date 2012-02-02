module Wares where

import Wrappers

data Ware = Fuel | Food | Supplies
    deriving (Eq, Show, Ord)

data Cargo = Cargo [(Ware, Amount)]
    deriving (Show)

defaultCargo = Cargo
               [ (Fuel,     0)
               , (Food,     0)
               , (Supplies, 0)
               ]

caprica = makeCargo [(Fuel, 30), (Supplies, 10)]

-- This function transforms a list of (Ware, Amount) tuples
-- into a list of closures that add (Ware, Amount) to a given Cargo
-- Then that list of closures is folded together with 
-- function composition (.) and applied to a default empty Cargo
-- This ensures that every type of cargo will be present
makeCargo :: [(Ware, Amount)] -> Cargo
makeCargo pairs =
    let fns = map ( \(w,a) -> 
                        \t -> addWare t w a) 
                  pairs
    in (foldl (.) id fns) defaultCargo

instance WareOps Cargo where
    addWare (Cargo ws) ware amount =
        let fn = \acc (w,a) -> if (w == ware) then acc ++ [(w,a+amount)]
                                              else acc ++ [(w,a)]
        in Cargo $ foldl fn [] ws
    enoughWare (Cargo ws) ware amount =
        let fn = \acc (w,a) -> if ((w == ware) && (a >= amount)) then True
                                                                 else acc
        in foldl fn False ws

class WareOps a where
    addWare :: a -> Ware -> Amount -> a
    removeWare :: a -> Ware -> Amount -> a
    enoughWare :: a -> Ware -> Amount -> Bool
    mbRemoveWare :: a -> Ware -> Amount -> Maybe a

    removeWare obj w a = addWare obj w (-a)
    mbRemoveWare obj w a = 
        if (enoughWare obj w a) then Just $ removeWare obj w a
                                else Nothing

class MoneyOps a where
    addMoney :: a -> Amount -> a
    removeMoney :: a -> Amount -> a
    enoughMoney :: a -> Amount -> Bool
    mbRemoveMoney :: a -> Amount -> Maybe a

    removeMoney obj a = addMoney obj (-a)
    mbRemoveMoney obj a = 
        if (enoughMoney obj a) then Just $ removeMoney obj a
                               else Nothing
