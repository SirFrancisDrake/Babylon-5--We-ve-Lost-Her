
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

instance Show Stock where -- Rather ugly code, probably needs revisiting. FIXME
    show sis = let nsis = ("Ware", "Amount", "Buying price", "Selling price", 
                          "Desired amount") : map (\(a,b,c,d,e) ->
                                 (show a, show b, show c, show d, show e)) 
                                 (sortBy (on compare si_ware) (filterTrading sis))
                   wareColWidth = (last . sort $ map length wareNames) + 1
                   lengthBy fn sis = (last . sort $ map (length . fn) nsis) + 1
                   amountColWidth        = lengthBy si_amount nsis
                   buyColWidth           = lengthBy si_buyPrice nsis
                   sellColWidth          = lengthBy si_sellPrice nsis
                   desiredAmountColWidth = lengthBy si_desiredAmount nsis
                   makeLength st ln = st ++ (take (ln - length st) $ repeat ' ')
                   showSi (w,a,bp,sp,da) = join $ intersperse "| " $ 
                     [ makeLength w wareColWidth
                     , makeLength a amountColWidth
                     , makeLength bp buyColWidth
                     , makeLength sp sellColWidth
                     , makeLength da desiredAmountColWidth
                     ]
                   header = join $ intersperse "| " $ 
                     [ makeLength "Ware" wareColWidth
                     , makeLength "Amount" amountColWidth
                     , makeLength "Buying price" buyColWidth
                     , makeLength "Selling price" sellColWidth
                     , makeLength "Desired amount" desiredAmountColWidth
                     ]
                   break = "\n\t" ++ (take (length header) $ repeat '-') ++ "\n"
               in "\t" ++ header ++ break ++ "\t" ++ 
                    (join $ intersperse "\n\t" (map showSi (tail nsis)))

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
