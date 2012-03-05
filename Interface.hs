module Interface where

import Control.Concurrent.STM
import Data.Char (toLower)
import Data.Maybe (isJust, fromJust)
import Data.IntMap hiding (filter, map)

import InterfaceShow
import Ships
import Stations
import StringFunctions
import Wares
import Wrappers
import World

data UserCommand = UCBack
                 | UCBuy Ware Amount
                 | UCCharacterInfo
                 | UCExit
                 | UCGoTo StationID
                 | UCList
                 | UCNavigation
                 | UCStationInfo
                 | UCSell Ware Amount
                 | UCTrade
                 | UCUndock
              -- | UCSave
    deriving ()

instance Show UserCommand where
    show (UCGoTo i) = "goto station " ++ show i
    show (UCSell w a) = "sell " ++ show a ++ " units of " ++ show w
    show (UCBuy w a) = "buy " ++ show a ++ " units of " ++ show w
    show UCUndock = "undock"
    show UCList = "list station cargo"
    show UCExit = "exit"

instance ContextualShow UserCommand where
    contextShow (c, _) UCBack = "back (to main interface screen)"
    contextShow (c, _) (UCBuy w a) = "buy (usage: `buy ware amount` or `buy amount ware`)"
    contextShow (c, _) UCCharacterInfo = "char (show character info)"
    contextShow (c, _) UCExit = "exit (the game)"
    contextShow (c, _) (UCGoTo i) = "go (to a station, usage: `go stationID`)"
    contextShow (c, _) UCList = "list (stock of the station or ship's cargo)"
    contextShow (c, _) UCNavigation = "navigation (go to navigation screen)"
    contextShow (c, _) (UCSell w a) = "sell (see buy)"
    contextShow (c, _) UCStationInfo = "info (about the given object)"
    contextShow (c, _) UCTrade = "trade (go to trade screen)"
    contextShow (c, _) UCUndock = "undock (from the station)"
    contextShowList i@(c, _) ts = "\n\t-> " ++ 
                concatWith "\n\t-> " (map (contextShow i) ts)

instance Eq UserCommand where
    (==) UCBack UCBack = True
    (==) (UCBuy _ _) (UCBuy _ _) = True
    (==) UCCharacterInfo UCCharacterInfo = True
    (==) UCExit UCExit = True
    (==) (UCGoTo _) (UCGoTo _) = True
    (==) UCList UCList = True
    (==) UCNavigation UCNavigation = True
    (==) UCStationInfo UCStationInfo = True
    (==) (UCSell _ _) (UCSell _ _) = True
    (==) UCTrade UCTrade = True
    (==) UCUndock UCUndock = True
    (==) _ _ = False

instance Recognize Int where
    recognize i = if filter (flip elem "1234567890") i == i then Just (read i)
                                                            else Nothing

instance Recognize UserCommand where
    recognize uc = let patterns = [ "back"
                                  , "buy"
                                  , "char"
                                  , "exit"
                                  , "go"
                                  , "info"
                                  , "list"
                                  , "navigation"
                                  , "sell"
                                  , "trade"
                                  , "undock"
                                  , "quit"
                                  ]
                   in case exhaustive (map toLower uc) patterns of
                       Just "back" -> Just UCBack
                       Just "buy" -> recognizeBuy $ words uc
                       Just "char" -> Just UCCharacterInfo
                       Just "exit" -> Just UCExit
                       Just "go" -> recognizeGo $ words uc
                       Just "info" -> Just UCStationInfo
                       Just "list" -> Just UCList
                       Just "navigation" -> Just UCNavigation
                       Just "sell" -> recognizeSell $ words uc
                       Just "trade" -> Just UCTrade
                       Just "undock" -> Just UCUndock
                       Just "quit" -> Just UCExit
                       otherwise -> Nothing

recognizeGo :: [String] -> Maybe UserCommand
recognizeGo (g:i:[]) = if (isJust $ (recognize i :: Maybe Int)) 
                            then Just $ UCGoTo (fromJust $ recognize i)
                            else Nothing
recognizeGo _ = Nothing

--      \ . /               | Rewrite those two when I get the chance
--       \|/                | that code is fucking terrible
--      --o--               | but "sell 10 fuel" and "sell fuel 10"
--        |                 | both work, which is very nice
--     kkk kkk            \ | /
--    0___0___0            \|/
recognizeSell :: [String] -> Maybe UserCommand
recognizeSell (_:w:a:[]) = let w1 = recognize w :: Maybe Ware
                               a1 = recognize a :: Maybe Amount
                               w2 = recognize a :: Maybe Ware
                               a2 = recognize w :: Maybe Amount
                           in if (isJust w1) && (isJust a1) then Just $ UCSell (fromJust w1) (fromJust a1)
                                                            else if (isJust w2) && (isJust a2) 
                                                                    then Just $ UCSell (fromJust w2) (fromJust a2)
                                                                    else Nothing
recognizeSell _ = Nothing

recognizeBuy :: [String] -> Maybe UserCommand
recognizeBuy (_:w:a:[]) = let w1 = recognize w :: Maybe Ware
                              a1 = recognize a :: Maybe Amount
                              w2 = recognize a :: Maybe Ware
                              a2 = recognize w :: Maybe Amount
                           in if (isJust w1) && (isJust a1) then Just $ UCSell (fromJust w1) (fromJust a1)
                                                            else if (isJust w2) && (isJust a2) 
                                                                    then Just $ UCBuy (fromJust w2) (fromJust a2)
                                                                    else Nothing
recognizeBuy _ = Nothing

printContext :: InterfaceState -> IO ()
printContext (ContextSpace, ScreenNavigation) = 
    putStrLn "You're in open space."
printContext ((ContextStationGuest stid), ScreenNavigation) = 
    putStrLn $ "You're on a station with ID " ++ show stid -- FIXME
printContext ((ContextStationOwner stid), ScreenNavigation) = 
    putStrLn $ "You're on a station with ID " ++ show stid -- FIXME
printContext ((ContextShipGuest shid), ScreenNavigation) = 
    putStrLn $ "You're on a ship with ID " ++ show shid -- FIXME
printContext _ = return ()

data UserResult = URSuccess
                | URFailure
                | URAnswer String
    deriving (Eq)

instance Show UserResult where
    show URSuccess = "Successful."
    show URFailure = "Unsuccessful."
    show (URAnswer s) = s

getValidCommand :: [UserCommand] -> IO UserCommand
getValidCommand ucs = do
    putStr "###> "
    comm <- getLine
    let c = recognize comm
    if (isJust c) && (fromJust c) `elem` ucs 
        then return (fromJust c)
        else putStrLn "Nah, wrong way, sorry. Try again\n"
             >> getValidCommand ucs

chooseAvailibleCommands :: InterfaceState -> [UserCommand]
chooseAvailibleCommands (ContextSpace, ScreenNavigation) =
    [ UCCharacterInfo
    , UCExit
    , UCGoTo 0
    ]
chooseAvailibleCommands ((ContextStationGuest st), ScreenMain) =
    [ UCCharacterInfo
    , UCExit
    , UCNavigation
    , UCStationInfo
    , UCTrade
    ]
chooseAvailibleCommands ((ContextStationGuest st), ScreenNavigation) =
    [ UCBack
    , UCCharacterInfo
    , UCExit
    , UCStationInfo
    , UCUndock
    ]
chooseAvailibleCommands ((ContextStationGuest st), ScreenTrade) =
    [ UCBack
    , UCBuy Fuel 1
    , UCCharacterInfo
    , UCExit
    , UCList
    , UCSell Fuel 1
    , UCStationInfo
    ]
chooseAvailibleCommands _ = undefined

printAvailibleCommands :: [UserCommand] -> InterfaceState -> IO ()
printAvailibleCommands ucs istate = 
    putStrLn $ "You can do something of the following: " ++ (contextShowList istate ucs)

getUserCommand :: InterfaceState -> IO (UserCommand, Bool)
getUserCommand istate = do
    let cmds = chooseAvailibleCommands istate
    printContext istate
    printAvailibleCommands cmds istate
    cmd <- getValidCommand cmds
    putStrLn $ contextShow istate cmd
    if cmd == UCExit then return (cmd, True)
                     else return (cmd, False)

executeUserCommand :: UserCommand -> InterfaceState -> World -> IO (UserResult, InterfaceState)
executeUserCommand UCStationInfo istate w = do
    stid <- (world_ships w) !!! 0 >>= return . dockedStID
    st <- (world_stations w) !!! stid
    return (URAnswer (contextShow istate st), istate)

executeUserCommand UCCharacterInfo istate w = do
    o <- (world_owners w) !!! 0
    return (URAnswer (show o), istate)

executeUserCommand UCList istate@(ContextStationGuest stid, ScreenTrade) w = do
   st <- (world_stations w) !!! stid
   return (URAnswer (show $ station_stock st), istate)

executeUserCommand UCUndock istate@(ContextStationGuest stid, ScreenNavigation) w = do
   tst <- readTVarIO (world_stations w) >>= \imapst -> return $ imapst ! stid
   shid <- getOwnerShipID w
   tsh <- readTVarIO (world_ships w) >>= \imapsh -> return $ imapsh ! shid
   atomically $ undockShSt tsh shid tst stid
   return (URSuccess, (ContextSpace, ScreenNavigation))

executeUserCommand UCUndock istate@(ContextStationOwner stid, ScreenNavigation) w = do
   tst <- readTVarIO (world_stations w) >>= \imapst -> return $ imapst ! stid
   shid <- getOwnerShipID w
   tsh <- readTVarIO (world_ships w) >>= \imapsh -> return $ imapsh ! shid
   atomically $ undockShSt tsh shid tst stid
   return (URSuccess, (ContextSpace, ScreenNavigation))

executeUserCommand UCExit istate _ = return (URSuccess, istate)
executeUserCommand UCBack istate _ = return (URSuccess, interface_back istate)
executeUserCommand UCTrade istate@(ContextStationGuest stid, _) w = do
   st <- (world_stations w) !!! stid
   return (URAnswer (show $ station_stock st), interface_trade istate)

executeUserCommand UCNavigation istate _ = return (URSuccess, interface_navigation istate)

executeUserCommand (UCBuy bw ba) istate@(ContextStationGuest stid, ScreenTrade) w = do
    applyTradeFn canBuy buy stid w bw ba >>= \r -> return (r, istate)

executeUserCommand (UCSell sw sa) istate@(ContextStationGuest stid, ScreenTrade) w = do
    applyTradeFn canSell sell stid w sw sa >>= \r -> return (r, istate)

executeUserCommand _ _ _ = undefined

-- Try to write down type declaration of this one. The answer can be found in Styleguide.lhs
applyTradeFn pred mod stid world w a = do
    oid <- getOwnerID world
    shid <- getOwnerShipID world
    canDo <- pred oid shid stid world w a
    if canDo then do mod oid shid stid world w a
                     return URSuccess
             else return URFailure


showResult :: UserResult -> IO ()
showResult = print

getOwnerShip :: World -> IO Ship
getOwnerShip w = (world_ships w) !!! 0 -- FIXME when needed

getOwnerShipID :: World -> IO ShipID
getOwnerShipID w = return 0 -- FIXME when needed

getOwnerID :: World -> IO OwnerID
getOwnerID _ = return 0 -- FIXME when needed

showSituation :: World -> InterfaceState -> IO ()
showSituation w istate = do
    putStr "-- Status: "
    ownerShip <- getOwnerShip w
    let nm = ship_navModule ownerShip
    let np = navModule_position nm
    case np of
        (DockedToStation stid) -> putStrLn "You seem to be docked to a station. "
        (DockedToShip shid) -> (world_ships w) !!! shid >>= print
        (SNPSpace pos) -> print pos

deviseContext :: World -> IO Context
deviseContext w = do
    ownerShip <- getOwnerShip w
    let nm = ship_navModule ownerShip
    let np = navModule_position nm
    case np of
        (DockedToStation stid) -> return $ ContextStationGuest stid
        (DockedToShip shid) -> return $ ContextShipGuest shid
        (SNPSpace pos) -> return $ ContextSpace
    
interfaceCycle :: World -> InterfaceState -> IO ()
interfaceCycle world istate = do
    showSituation world istate
    (command, ifStop) <- getUserCommand istate
    (result, newIstate) <- executeUserCommand command istate world
    showResult result
    putStrLn ""
    if not ifStop then interfaceCycle world newIstate
                  else return ()
