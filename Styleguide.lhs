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

2. Unclear code > minor code duplication

Consider this code (Interface.hs):
> executeUserCommand :: UserCommand -> InterfaceState -> World -> IO (UserResult, InterfaceState)
> executeUserCommand (UCBuy bw ba) istate@(ContextStationGuest stid, ScreenTrade) w = do
>     applyTradeFn canBuy buy stid bw ba >>= \r -> return (r, istate)
> 
> executeUserCommand (UCSell sw sa) istate@(ContextStationGuest stid, ScreenTrade) w = do
>     applyTradeFn canSell sell stid sw sa >>= \r -> return (r, istate)
>
> applyTradeFn pred mod stid world w a = do
>     oid <- getOwnerID world
>     shid <- getOwnerShipID world
>     canDo <- pred oid shid stid world w a
>     if canDo then do mod oid shid stid world w a
>                      return (URSuccess, istate)
>              else return (URFailure, istate)

It could also be written like this:
> executeUserCommand :: UserCommand -> InterfaceState -> World -> IO (UserResult, InterfaceState)
> executeUserCommand (UCBuy bw ba) istate@(ContextStationGuest stid, ScreenTrade) w = do
>     oid <- getOwnerID w
>     shid <- getOwnerShipID w
>     cb <- canBuy oid shid stid w bw ba
>     if cb then do buy oid shid stid w bw ba
>                   return (URSuccess, istate)
>           else return (URFailure, istate)
> 
> executeUserCommand (UCSell sw sa) istate@(ContextStationGuest stid, ScreenTrade) w = do
>     oid <- getOwnerID w
>     shid <- getOwnerShipID w
>     cs <- canSell oid shid stid w sw sa
>     if cs then do sell oid shid stid w sw sa
>                   return (URSuccess, istate)
>           else return (URFailure, istate)

16 lines for the first attempt, 16 for the second. One could hang himself trying to
come up with type declaration for applyTradeFn (which I provide below), but it definitely
looks better this way.
> -- I am enjoying this type declaration very much. Aren't you?
> applyTradeFn :: (OwnerID -> ShipID -> StationID -> World -> Ware -> Amount -> IO Bool) ->
>                 (OwnerID -> ShipID -> StationID -> World -> Ware -> Amount -> IO ()) ->
>                 StationID -> World -> Ware -> Amount -> IO UserResult
