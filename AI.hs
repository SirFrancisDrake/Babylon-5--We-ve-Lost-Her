module AI where

import Control.Concurrent.STM
import Control.Monad.Reader

import Auxiliary.Zipper
import Contexts
import Currency
import DataTypes
import Owner
import Ships
import TradeIO
import Wares
import Wrappers

              -- where2buy        where2sell
defSupplyAI :: (TVar Station) -> (TVar Station) -> Ware -> Amount -> ShipAI
defSupplyAI btst stst w a = 
  ShipAI (fromList
           [ SGo btst
           , SBuy w a
           , SGo stst
           , SSell w a
           ])

defaultAI = SAINone

runByAI :: Ship -> Bool
runByAI sh =
  case ship_AI sh of
    ShipAI _ -> True
    otherwise -> False

ai_current :: ShipAI -> SCommand
ai_current (ShipAI z) = zip_current z
ai_current _ = error "Can't AI current this"

ai_next :: ShipAI -> SCommand
ai_next (ShipAI z) = head $ zip_others z
ai_next _ = error "Can't AI current this"

ai_rotn :: Int -> ShipAI -> ShipAI
ai_rotn 0 s = s
ai_rotn i (ShipAI z) = ai_rotn (i-1) (ShipAI $ rotate z)

processAI :: TVar Ship -> ReaderT World STM ()
processAI tsh = do
  sh <- lift $ readTVar tsh
  let ai = ship_AI sh
      to = ship_owner sh
  case ai_current ai of
    (SGo tst) -> 
      case dockedM sh of
        (Just tst) -> 
          case ai_next ai of
            SBuy w a -> do
              t <- return $ genTradeContext to tsh tst
              lift $ runReaderT (buy w a) t
              lift $ writeTVar tsh sh{ ship_AI = ai_rotn 2 ai }
            SSell w a -> do
              t <- return $ genTradeContext to tsh tst
              lift $ runReaderT (sell w a) t
              lift $ writeTVar tsh sh{ ship_AI = ai_rotn 2 ai }
        otherwise -> return ()
    otherwise -> undefined
          
