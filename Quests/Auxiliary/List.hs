
module Auxiliary.List where

import Data.Function (on)
import Data.List (groupBy, sortBy)

merge :: (Ord k, Num v) => [(k,v)] -> [(k,v)] -> [(k,v)]
merge xs ys = 
    let addTuples (a1,a2) (_,b2) = (a1, a2 + b2)
        sumTuples list = foldr addTuples (head list) (tail list)
        xys = sortBy (on compare fst) (xs ++ ys)
    in map sumTuples (groupBy (on (==) fst) xys)
