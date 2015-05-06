
module Auxiliary.Map where

import qualified Control.Monad as CM (filterM)
import Data.Map

filterM :: (Monad m, Ord k) => (v -> m Bool) -> Map k v -> m (Map k v)
filterM fn m =
  CM.filterM (\(a,b) -> fn b) (toList m) >>= return . fromList

