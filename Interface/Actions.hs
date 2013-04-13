
module Interface.Actions where

import Wares
import Wrappers

data TradeAction =
  Buy TradeArgs | Sell TradeArgs
  deriving (Show)

type TradeArgs = (Ware, Amount)

