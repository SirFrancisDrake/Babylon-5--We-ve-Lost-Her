
{-# LANGUAGE TypeSynonymInstances #-}

module Stock where

import Control.Monad (join)
import Data.Function (on)
import Data.List (foldl', intersperse, sort, sortBy)

import Wares
import Wrappers

type StockItem = (Ware, Amount, BuyPrice, SellPrice, DesiredAmount)
type Stock = [StockItem]

si_ware           (w,_,_,_,_) = w
si_amount         (_,a,_,_,_) = a
si_buyPrice      (_,_,bp,_,_) = bp
si_sellPrice     (_,_,_,sp,_) = sp
si_desiredAmount (_,_,_,_,da) = da

standardBuyPriceDeparture :: Double
standardBuyPriceDeparture = 0.9

standardSellPriceDeparture :: Double
standardSellPriceDeparture = 1.1

setStockPrice :: Stock -> Stock
setStockPrice stis = map setStockItemPrice stis

setStockItemPrice :: StockItem -> StockItem
setStockItemPrice (w,ta,_,_,da) =
    let a = if ta == 0 then 1
                       else ta
    in (w
       , ta
       , round $ 
       fromIntegral da / fromIntegral a 
       * fromIntegral (standardPrice w)
       * standardBuyPriceDeparture
       , round $ 
       fromIntegral da / fromIntegral a 
       * fromIntegral (standardPrice w)
       * standardSellPriceDeparture
       , da
       )

defaultStock = let fn (w,a) = (w,a,0,0,0)
               in map fn (fromCargo defaultCargo)

makeStock :: [(Ware, Amount, BuyPrice, SellPrice, DesiredAmount)] -> Stock
makeStock tuples =
    let fns = map ( \(w,a,bp,sp,da) -> 
                        \st -> (addWare w a) (setDesired st w da))
                  tuples
    in (foldl' (.) id (setStockPrice:fns)) defaultStock

setDesired :: Stock -> Ware -> DesiredAmount -> Stock
setDesired st ware desa =
    let fn = \acc i@(w,a,_,_,da) -> if (w == ware) then acc ++ [(w,a,0,0,desa)]
                                                   else acc ++ [i]
    in foldl' fn [] st

filterTrading :: Stock -> Stock
filterTrading sis = filter (\(_,_,_,_,da) -> da /= 0) sis

instance WareOps Stock where
    addWare ware amount st =
        let fn = \acc i@(w,a,_,_,da) -> if (w == ware) then acc ++ [setStockItemPrice (w,a+amount,0,0,da)]
                                                       else acc ++ [i]
        in foldl' fn [] st
    enoughWare ware amount st =
        let fn = \acc (w,a,_,_,_) -> if ((w == ware) && (a >= amount)) then True
                                                                       else acc
        in foldl' fn False st
    checkWare ware st =
        let fn = \acc (w,a,_,_,_) -> if (w == ware) then a
                                                    else acc
        in foldl' fn (-1) st

instance StockOps Stock where
    stockBuyPrice stis ware =
        let fn = \acc (w,_,bp,_,_) -> if (w == ware) then bp
                                                     else acc
        in foldl' fn 0 stis
    stockSellPrice stis ware =
        let fn = \acc (w,_,_,sp,_) -> if (w == ware) then sp
                                                     else acc
        in foldl' fn 0 stis

class StockOps a where
    stockBuyPrice :: a -> Ware -> Money
    stockSellPrice :: a -> Ware -> Money
