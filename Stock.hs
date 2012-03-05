module Stock where

import Control.Monad (join)
import Data.List (foldl', intersperse, sort)

import Wares
import Wrappers

type StockItem = (Ware, Amount, BuyPrice, SellPrice, DesiredAmount)
data Stock = Stock [StockItem]

si_ware           (w,_,_,_,_) = w
si_amount         (_,a,_,_,_) = a
si_buyPrice      (_,_,bp,_,_) = bp
si_sellPrice     (_,_,_,sp,_) = sp
si_desiredAmount (_,_,_,_,da) = da

instance Show Stock where -- Rather ugly code, probably needs revisiting. FIXME
    show (Stock sis) = let nsis = ("Ware", "Amount", "Buying price", "Selling price", 
                                  "Desired amount") : map (\(a,b,c,d,e) ->
                                    (show a, show b, show c, show d, show e)) sis
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
setStockPrice (Stock stis) = Stock $ map setStockItemPrice stis

setStockItemPrice :: StockItem -> StockItem
setStockItemPrice (w,a,_,_,da) = (w
                                 , a
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
               in Stock $ map fn (fromCargo defaultCargo)

instance WareOps Stock where
    addWare ware amount (Stock st) =
        let fn = \acc i@(w,a,_,_,da) -> if (w == ware) then acc ++ [setStockItemPrice (w,a+amount,0,0,da)]
                                                       else acc ++ [i]
        in Stock $ foldl' fn [] st
    enoughWare ware amount (Stock st) =
        let fn = \acc (w,a,_,_,_) -> if ((w == ware) && (a >= amount)) then True
                                                                       else acc
        in foldl' fn False st
    checkWare ware (Stock st) =
        let fn = \acc (w,a,_,_,_) -> if (w == ware) then a
                                                    else acc
        in foldl' fn (-1) st
