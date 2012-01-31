module IntMapAux where

import Data.IntMap
import Prelude hiding (null) -- looks like we need a `container` typeclass

-- This function inserts a value into an IntMap without a pre-defined key
-- The key it invents by itself. Object instance IDs come from this fn
insertMax :: a -> IntMap a -> IntMap a
insertMax val imap 
    | null imap = insert 0 val imap
    | otherwise = let key = fst (findMax imap) + 1
                  in insert key val imap
-- note that this code is potentially imperfect since it's possible
-- to have an IntMap like "fromList [(2^480, someval)], and this fn
-- will tend to add long-long keys instead of [1..] \\ [2^480]
