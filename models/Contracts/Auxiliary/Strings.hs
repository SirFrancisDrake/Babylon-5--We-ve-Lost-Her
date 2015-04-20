
module Auxiliary.Strings where

import Control.Monad (join)
import Data.List (intersperse)

concatWith inter items = join $ intersperse inter items 
