
{-# LANGUAGE TypeSynonymInstances #-}

module Stock where

import Control.Monad (join)
import Data.Function (on)
import Data.List (foldl', intersperse, sort, sortBy)
import qualified Data.Map as M

import GlobalConst (const_pricing_buyToFair, const_pricing_sellToFair)
import Wares
import Wrappers

sigmoid t = 1 / (1 + exp( (t - 0.5)*5.46 ) )
--           1
-- --------------------
--         (t - 0.5) / 2 
--   1 + e
--
-- /|\
--1 | 
--  |--.
--  |   -
--  |    -
--  |     -..
--  \--------------->
--      1/2

fairPrice :: Price -> Price -> Amount -> Amount -> Price
fairPrice minp maxp currentAmount maxAmount =
  let f = fromIntegral
      amountDeparture = (f currentAmount) / (f maxAmount)
  in  round $ (f minp) + (f (maxp - minp)) * sigmoid( amountDeparture )

modFairPrice :: (RealFrac a) => 
  (a -> Int) -> a -> Price -> Price -> Amount -> Amount -> Price
modFairPrice roundFn p minp maxp ca ma = 
  roundFn . (* p) . fromIntegral $ fairPrice minp maxp ca ma

buyPrice  = modFairPrice floor   const_pricing_buyToFair
sellPrice = modFairPrice ceiling const_pricing_sellToFair

-- Buy  price as in "entity buys"
-- Sell price as in "entity sells"
instance StockOps Stock where
  stockBuyPrice stock ware amount =
    case M.lookup ware stock of
      Nothing -> error $ "StockOps: stockBuyPrice: entity doesn't trade in " ++ show ware
      Just st ->
        case si_type st of
          StockItemBuying ->
            let ca = checkWarePure ware stock
                cm = si_maxStock st
                minp = si_minPrice st
                maxp = si_maxPrice st
            in  buyPrice minp maxp ca cm

          StockItemBoth -> 
            let ca = checkWarePure ware stock
                cm = si_maxStock st
                minp = si_minPrice st
                maxp = si_maxPrice st
                avg ns = (sum ns) `div` (length ns)
            in avg [ buyPrice minp maxp ca            cm 
                   , buyPrice minp maxp (ca + amount) cm]

          StockItemSelling -> error $ "StockOps: stockBuyPrice: entity only sells " ++ show ware

  stockSellPrice stock ware amount =
    case M.lookup ware stock of
      Nothing -> error $ "StockOps: stockSellPrice: entity doesn't trade in " ++ show ware
      Just st ->
        case si_type st of
          StockItemSelling ->
            let ca = checkWarePure ware stock
                cm = si_maxStock st
                minp = si_minPrice st
                maxp = si_maxPrice st
            in  sellPrice minp maxp ca cm

          StockItemBoth -> 
            let ca = checkWarePure ware stock
                cm = si_maxStock st
                minp = si_minPrice st
                maxp = si_maxPrice st
                avg ns = (sum ns) `div` (length ns)
            in avg [ sellPrice minp maxp ca           cm 
                   , sellPrice minp maxp (ca - amount) cm]

          StockItemBuying -> error $ "StockOps: stockSellPrice: entity only buys " ++ show ware

data StockItem = StockItem
  { si_amount :: Amount
  , si_maxStock :: Amount
  , si_minPrice :: Price
  , si_maxPrice :: Price
  , si_type :: StockItemType
  }
  deriving ()

data StockItemType =
  StockItemBoth 
  | StockItemBuying
  | StockItemSelling 

makeStock :: [ (Ware, Price, Price, Amount, Amount) ] -> StockItemType -> Stock
makeStock ps sit =
  let makeItem (w, pmin, pmax, ca, ma) =
        (w, StockItem ca ma pmin pmax sit)
  in M.fromList (map makeItem ps)

makeEmptyStock :: [ (Ware, Price, Price, Amount ) ] -> StockItemType -> Stock
makeEmptyStock ps sit =
  let makeItem (w, pmin, pmax, ma) = 
        (w, StockItem 0 ma pmin pmax sit)
  in M.fromList (map makeItem ps)

makeProducer :: [ (Ware, Price, Price, Amount, Amount) ] -> Stock
makeProducer = flip makeStock StockItemSelling

makeConsumer :: [ (Ware, Price, Price, Amount, Amount) ] -> Stock
makeConsumer = flip makeStock StockItemBuying

type Stock = M.Map Ware StockItem

instance WareOps Stock where
    addWarePure ware amount st =
      let mw = M.lookup ware st
      in  case mw of
            Just item ->
              let upd = item{ si_amount = (si_amount item + amount) }
              in  M.update (\_ -> return upd) ware st
            Nothing -> error "WareOps: addWarePure: Can't find a ware in stock"
    checkWarePure ware st =
      let mw = M.lookup ware st
      in  case mw of
            Just item -> si_amount item
            Nothing -> error "WareOps: checkWarePure: Can't find a ware in stock"

class StockOps a where
    stockBuyPrice :: a -> Ware -> Amount -> Money
    stockSellPrice :: a -> Ware -> Amount -> Money

    stockBuyTotal :: a -> Ware -> Amount -> Money
    stockBuyTotal st w a = a * (stockBuyPrice st w a)

    stockSellTotal :: a -> Ware -> Amount -> Money
    stockSellTotal st w a = a * (stockSellPrice st w a)
