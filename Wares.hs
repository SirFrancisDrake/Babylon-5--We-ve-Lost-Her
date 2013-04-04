module Wares where

import Control.Concurrent.STM
import Control.Monad (join)
import Data.Char (toLower)
import Data.Function (on)
import Data.List (foldl', intersperse, sort, sortBy)
import Data.Monoid

import Auxiliary.StringFunctions
import Auxiliary.Transactions
import Wrappers

data Ware = Books | CyberModules | Energy | Food | Fuel | Silicium | Supplies
    deriving (Enum, Eq, Show, Ord)

allWares  = [Books .. Supplies]
wareNames = map show [Books .. Supplies]

standardPrice :: Ware -> Money
standardPrice Books = 10
standardPrice CyberModules = 300
standardPrice Energy = 7
standardPrice Food = 15
standardPrice Fuel = 14
standardPrice Silicium = 90
standardPrice Supplies = 20

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
    deriving ()

instance Show Cargo where -- copypasted from Stock.hs `instance Show Stock`, generalize FIXME
    show (Cargo ps) = let nps = ("Ware", "Amount") : map (\(a,b) ->
                                         (show a, show b)) 
                                         (sortBy (on compare fst) ps)
                          wareColWidth = (last . sort $ map length wareNames) + 1
                          lengthBy fn ps = (last . sort $ map (length . fn) nps) + 1
                          amountColWidth        = lengthBy snd nps
                          makeLength st ln = st ++ (take (ln - length st) $ repeat ' ')
                          showP (w,a) = join $ intersperse "| " $ 
                            [ makeLength w wareColWidth
                            , makeLength a amountColWidth
                            ]
                          header = join $ intersperse "| " $ 
                            [ makeLength "Ware" wareColWidth
                            , makeLength "Amount" amountColWidth
                            ]
                          break = "\n\t" ++ (take (length header) $ repeat '-') ++ "\n"
                      in "\t" ++ header ++ break ++ "\t" ++ 
                           (join $ intersperse "\n\t" (map showP (tail nps)))

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
                        \t -> addWarePure w a t) 
                  pairs
    in (foldl' (.) id fns) defaultCargo

fromCargo :: Cargo -> [(Ware, Amount)]
fromCargo (Cargo ws) = ws

caprica = makeCargo [(Fuel, 30), (Supplies, 10)]

exchangeWare :: Cargo -> Ware -> Amount -> Ware -> Amount -> Cargo
exchangeWare c wc ac wp ap =
    if enoughWarePure wc ac c then (removeWarePure wc ac (addWarePure wp ap c))
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
    addWarePure ware amount (Cargo ws) =
        let fn = \acc (w,a) -> if (w == ware) then acc ++ [(w,a+amount)]
                                              else acc ++ [(w,a)]
        in Cargo $ foldl' fn [] ws
    enoughWarePure ware amount (Cargo ws) =
        let fn = \acc (w,a) -> if ((w == ware) && (a >= amount)) then True
                                                                 else acc
        in foldl' fn False ws
    checkWarePure ware (Cargo ws) =
        let fn = \acc (w,a) -> if (w == ware) then a
                                              else acc
        in foldl' fn (-1) ws

class WareOps a where
    addWarePure      :: Ware -> Amount -> a -> a
    checkWarePure    :: Ware -> a -> Amount
    removeWarePure   :: Ware -> Amount -> a -> a
    enoughWarePure   :: Ware -> Amount -> a -> Bool
    mbRemoveWarePure :: Ware -> Amount -> a -> Maybe a

    addWare    :: Ware -> Amount -> TVar a -> STM ()
    addWare w a = modifyT (addWarePure w a)

    checkWare  :: Ware -> TVar a -> STM Amount
    checkWare w = checkT (checkWarePure w)

    removeWare :: Ware -> Amount -> TVar a -> STM ()
    removeWare w a = modifyT (removeWarePure w a)

    enoughWare :: Ware -> Amount -> TVar a -> STM Bool
    enoughWare w a = checkT (enoughWarePure w a)

    removeWarePure w a obj = addWarePure w (-a) obj
    mbRemoveWarePure w a obj = 
        if (enoughWarePure w a obj) then Just $ removeWarePure w a obj
                                else Nothing

