
module TradeIO where

import Control.Concurrent.STM
import Control.Monad.Reader

import Contexts
import Currency
import DataTypes
import Owner
import Ships
import Stock
import Wares
import Wrappers

canBuy :: Ware -> Amount -> ReaderT TradeContext STM Bool
canBuy bw ba = do
  (to, tsh, tst) <- ask
  sh <- lift $ readTVar tsh
  st <- lift $ readTVar tst
  o <- lift $ readTVar to
  let enoughSpaceP = ship_freeSpace sh >= weight bw * fromIntegral ba
  let enoughMoneyP = owner_money o >= stockSellTotal st bw ba
  let enoughWareP = enoughWarePure bw ba st
  return $ and [enoughSpaceP, enoughMoneyP, enoughWareP] 

buy :: Ware -> Amount -> ReaderT TradeContext STM ()
buy bw ba = do
  (to, tsh, tst) <- ask
  st <- lift $ readTVar tst
  cb <- canBuy bw ba

  if cb then lift $ do
          removeWare bw ba tst
          addWare bw ba tsh
          removeMoney (stockSellTotal st bw ba) to
          addMoney (stockSellTotal st bw ba) tst
        else return ()

canSell :: Ware -> Amount -> ReaderT TradeContext STM Bool
canSell sw sa = do
  (to, tsh, tst) <- ask
  o <- lift $ readTVar to
  sh <- lift $ readTVar tsh
  st <- lift $ readTVar tst
  let enoughMoneyP = station_money st >= stockBuyTotal st sw sa
  let enoughWareP = enoughWarePure sw sa sh

  return $ and [enoughMoneyP, enoughWareP] 

sell :: Ware -> Amount -> ReaderT TradeContext STM ()
sell sw sa = do
  (to, tsh, tst) <- ask
  st <- lift $ readTVar tst
  cs <- canSell sw sa

  if cs then lift $ do
          removeWare sw sa tsh
          addWare sw sa tst
          removeMoney (stockBuyTotal st sw sa) tst
          addMoney (stockBuyTotal st sw sa) to
        else return ()
