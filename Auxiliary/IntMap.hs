module Auxiliary.IntMap where

import Data.IntMap hiding ((\\),map)
import Data.List ((\\))
import Prelude hiding (filter, null) -- looks like haskell needs a `container` typeclass

-- This function inserts a value into an IntMap without a pre-defined key
-- The key it invents by itself. Object instance IDs come from this fn
insertMax :: a -> IntMap a -> IntMap a
insertMax val imap 
    | null imap = insert 0 val imap
    | otherwise = let key = fst (findMax imap) + 1 --key = head $ [0..] \\ (keys imap) 
                  in insert key val imap
-- note that this code is potentially imperfect since it's possible
-- to have an IntMap like "fromList [(2^480, someval)], and this fn
-- will tend to add long-long keys instead of [1..] \\ [2^480]

lookupUnique :: (Eq a) => a -> IntMap a -> Maybe a
lookupUnique a as = let filtered = toList $ filter (== a) as
                    in if filtered == [] then Nothing
                                         else Just $ snd $ head filtered

vals :: IntMap a -> [a]
vals = elems
