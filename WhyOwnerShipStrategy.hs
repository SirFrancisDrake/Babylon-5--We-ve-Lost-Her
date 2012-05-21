
assignOwner :: (TVar Ship) -> (TVar Owner) -> STM ()
assignOwner tsh to = do
    readTVar tsh >>= \i -> writeTVar tsh ( i{ ship_owner = to } )

makeShip :: Ship -> 

data Ship = Ship
  { ship_name :: String
  , ship_owner :: Either String (TVar Owner)
  }

create :: [Ship] -> [TVar Ship]
create ships = do
    tships <- mapM createShip ships
    mapM findAndSetOwner tships
    where findAndSetOwner s = s{ ship_owner = Right (getTVarOwner $ fromLeft $ ship_owner s }

  -- OR
data Ship = Ship
  { ship_name :: String
  , ship_owner :: TVar Owner
  }

create :: [ (Ship, TVar Owner) ] -> [TVar Ship]
create sown =
    mapM (\(s,o) -> createShip s{ ship_owner = o }) sown

-- example data entry
draziShips :: [Ship]
draziShips = ...

createAllShips = do
    tdrazi <- makeOwner drazi
    tdraziShips <- create $ zip draziShips (repeat tdrazi)

-- Second looks much better because I don't kill the code to make my life easier

-- Second problem
-- Variant 1

drazi :: Owner
drazi = ...

draziShips :: [Ship]
draziShips = [...]

everything = [ (drazi, draziShips)
             , ...
             [

-- Variant 2

drazi :: (Owner, [Ship])
drazi = ( Owner ..., [ ... ] )

makeRaces :: [ (Owner, [Ship]) ] -> STM ([TVar Owner], [TVar Ship])
makeRaces list = 
    mapM (\(o, ss) -> makeOwner o >>= \t -> (t, ss)) list
    >>=
    mapM (\(to, ss) -> map makeShip (zip (repeat to) ss))

makeWorld :: [ (Owner, [Ship]) ] -> [Station] -> STM World
makeWorld races stations = do
    (towners, tships) <- makeRaces races
    tstations <- makeStations stations
    newTVar $ World ships owners stations
