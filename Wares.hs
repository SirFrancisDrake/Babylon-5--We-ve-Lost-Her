module Wares where

import Data.Char (toLower)
import Data.List (foldl')
import Data.Monoid

import StringFunctions
import Wrappers

data Ware = Books | Energy | Fuel | Food | Supplies | CyberModules | Silicium
    deriving (Enum, Eq, Show, Ord)

instance Recognize Ware where
    recognize w = let patterns = [ "books"
                                 , "energy"
                                 , "fuel"
                                 , "food"
                                 , "supplies"
                                 , "cybermodules"
                                 , "silicium"
                                 ]
                  in case exhaustive (map toLower w) patterns of
                        Just "books" -> Just Books
                        Just "energy" -> Just Energy
                        Just "fuel" -> Just Fuel
                        Just "food" -> Just Food
                        Just "supplies" -> Just Supplies
                        Just "cybermodules" -> Just CyberModules
                        Just "silicium" -> Just Silicium
                        otherwise -> Nothing
                    

data Cargo = Cargo [(Ware, Amount)]
    deriving (Show)

defaultCargo = Cargo
               [ (Fuel,         0)
               , (Books,        0)
               , (Energy,       0)
               , (Food,         0)
               , (Supplies,     0)
               , (CyberModules, 0)
               , (Silicium,     0)
               ]

-- This function transforms a list of (Ware, Amount) tuples
-- into a list of closures that add (Ware, Amount) to a given Cargo
-- Then that list of closures is folded together with 
-- function composition (.) and applied to a default empty Cargo
-- This ensures that every type of cargo will be present
makeCargo :: [(Ware, Amount)] -> Cargo
makeCargo pairs =
    let fns = map ( \(w,a) -> 
                        \t -> addWare w a t) 
                  pairs
    in (foldl' (.) id fns) defaultCargo

caprica = makeCargo [(Fuel, 30), (Supplies, 10)]

exchangeWare :: Cargo -> Ware -> Amount -> Ware -> Amount -> Cargo
exchangeWare c wc ac wp ap =
    if enoughWare wc ac c then (removeWare wc ac (addWare wp ap c))
                          else c

exchangeWareTimes :: Cargo -> Ware -> Amount -> Ware -> Amount -> Int -> Cargo
exchangeWareTimes c wc ac wp ap 0 = c
exchangeWareTimes c wc ac wp ap t = exchangeWareTimes (exchangeWare c wc ac wp ap) wc ac wp ap (t-1)
-- This is totally not elegant and inefficient FIXME
-- need it to let factories work with half their capacity and so on

instance Weighable Cargo where
    weight (Cargo c) = foldl' (\acc (w,a) -> 
                                weight w * fromIntegral a + acc) 
                              0 
                              c

instance Weighable Ware where
    weight w
        | w == Fuel         = 0.3
        | w == Books        = 0.5
        | w == Energy       = 0.1
        | w == Food         = 0.24
        | w == Supplies     = 0.4
        | w == CyberModules = 0.05
        | w == Silicium     = 1

class Weighable a where
    weight :: a -> Weight

instance WareOps Cargo where
    addWare ware amount (Cargo ws) =
        let fn = \acc (w,a) -> if (w == ware) then acc ++ [(w,a+amount)]
                                              else acc ++ [(w,a)]
        in Cargo $ foldl' fn [] ws
    enoughWare ware amount (Cargo ws) =
        let fn = \acc (w,a) -> if ((w == ware) && (a >= amount)) then True
                                                                 else acc
        in foldl' fn False ws
    checkWare ware (Cargo ws) =
        let fn = \acc (w,a) -> if (w == ware) then a
                                              else acc
        in foldl' fn (-1) ws

class WareOps a where
    addWare :: Ware -> Amount -> a -> a
    checkWare :: Ware -> a -> Amount
    removeWare :: Ware -> Amount -> a -> a
    enoughWare :: Ware -> Amount -> a -> Bool
    mbRemoveWare :: Ware -> Amount -> a -> Maybe a

    removeWare w a obj = addWare w (-a) obj
    mbRemoveWare w a obj = 
        if (enoughWare w a obj) then Just $ removeWare w a obj
                                else Nothing

