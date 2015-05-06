
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Stock where

import Control.Monad (join)
import Data.Function (on)
import Data.List (foldl', intersperse, sort, sortBy)
import qualified Data.Map as M
import Data.Maybe (fromJust)

import GlobalConst (const_deficit, const_abundancy
                   , const_pricing_buyToFair, const_pricing_sellToFair)
import Wares
import Wrappers

data PricingModel = PricingModel
  { pricing_bothBuy      :: PricingFn
  , pricing_bothSell     :: PricingFn
  , pricing_consumerBuy  :: PricingFn
  , pricing_producerSell :: PricingFn
  }

            -- minprice maxprice  current max_possible amount_customer_wants
type PricingFn = Price -> Price -> Amount -> Amount -> Amount -> Price

-- calls a pricing function ignoring the amount_customer_wants part
simplified fn a b c d _ = fn a b c d

abundancyDeficitPricing :: PricingModel
abundancyDeficitPricing = PricingModel
  { pricing_bothBuy      = mixedBuy
  , pricing_consumerBuy  = simplified buyPrice
  , pricing_bothSell     = mixedSell
  , pricing_producerSell = simplified sellPrice
  }

portRoyalePricing :: PricingModel
portRoyalePricing = PricingModel
  { pricing_bothBuy      =   deficitPricing
  , pricing_consumerBuy  =   deficitPricing
  , pricing_bothSell     = abundancyPricing
  , pricing_producerSell = abundancyPricing
  }

xTensionPricing :: PricingModel
xTensionPricing = PricingModel
  { pricing_bothBuy      = simplified buyPrice 
  , pricing_consumerBuy  = simplified buyPrice 
  , pricing_bothSell     = simplified sellPrice
  , pricing_producerSell = simplified sellPrice
  }

globalPricing = abundancyDeficitPricing

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

avg xs = sum xs `div` length xs

deficitPricing :: Price -> Price -> Amount -> Amount -> Amount -> Price
deficitPricing minp maxp acur amax atosell =
  avg [ sellPrice minp maxp       acur       amax  
      , sellPrice minp maxp (acur - atosell) amax ]

standardPricingSell :: Price -> Price -> Amount -> Amount -> Price
standardPricingSell = sellPrice

mixedSell :: Price -> Price -> Amount -> Amount -> Amount -> Price
mixedSell minp maxp acur amax atosell =
  if (fromIntegral acur) / (fromIntegral amax) < const_deficit
    then deficitPricing minp maxp acur amax atosell
    else standardPricingSell minp maxp acur amax

abundancyPricing :: Price -> Price -> Amount -> Amount -> Amount -> Price
abundancyPricing minp maxp acur amax atobuy =
  avg [ buyPrice minp maxp       acur       amax  
      , buyPrice minp maxp (acur + atobuy) amax ]

standardPricingBuy :: Price -> Price -> Amount -> Amount -> Price
standardPricingBuy = buyPrice

mixedBuy :: Price -> Price -> Amount -> Amount -> Amount -> Price
mixedBuy minp maxp acur amax atobuy =
  if (fromIntegral acur) / (fromIntegral amax) > const_abundancy
    then abundancyPricing minp maxp acur amax atobuy
    else standardPricingBuy minp maxp acur amax

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
          StockItemBoth -> callFnOnStock stock (pricing_bothBuy globalPricing) ware amount
          StockItemBuying -> callFnOnStock stock (pricing_consumerBuy globalPricing) ware amount
          StockItemSelling -> error $ "StockOps: stockBuyPrice: entity only sells " ++ show ware

  stockSellPrice stock ware amount =
    case M.lookup ware stock of
      Nothing -> error $ "StockOps: stockSellPrice: entity doesn't trade in " ++ show ware
      Just st ->
        case si_type st of
          StockItemBoth -> callFnOnStock stock (pricing_bothSell globalPricing) ware amount
          StockItemBuying -> error $ "StockOps: stockSellPrice: entity only buys " ++ show ware
          StockItemSelling -> callFnOnStock stock (pricing_producerSell globalPricing) ware amount

callFnOnStock :: Stock -> PricingFn -> Ware -> Amount -> Price
callFnOnStock stock fn w a =
  let si = fromJust (M.lookup w stock) -- fromJust is safe because of the above check
      ca = checkWarePure w stock
      cm = si_maxStock si
      minp = si_minPrice si
      maxp = si_maxPrice si
  in  fn minp maxp ca cm a

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
