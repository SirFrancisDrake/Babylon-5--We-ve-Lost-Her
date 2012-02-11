> import Data.IntMap
> import Prelude hiding (map, filter)

1. Lambda > flip
example:
We want to run function `process` on a subset of IntMap's elements.
Suppose the we know that subset by the list of its keys.

> -- process :: Int -> IntMap a -> a
> process = undefined
> ex1  intMap keyList = map (\k -> process k intMap) keyList
> ex1' intMap keyList = map (flip process intMap) keyList

Rewriting `process` fn to take args in different order is also possible,
but not always desirable. 
For me, a lambda here is much easier to understand than a flip, although
it looks a bit less hacky, but oh well.
