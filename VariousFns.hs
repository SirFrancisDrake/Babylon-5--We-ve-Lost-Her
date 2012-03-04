module VariousFns where

matchAllPredicates :: [(a -> Bool)] -> a -> Bool
matchAllPredicates fns a = and $ map (\f -> f a) fns

mapMatchAllPredicates :: [(a -> Bool)] -> [a] -> Bool
mapMatchAllPredicates fns as = and $ map (matchAllPredicates fns) as

matchAnyPredicates :: [(a -> Bool)] -> a -> Bool
matchAnyPredicates fns a = or $ map (\f -> f a) fns

mapMatchAnyPredicates :: [(a -> Bool)] -> [a] -> Bool
mapMatchAnyPredicates fns as = or $ map (matchAnyPredicates fns) as

