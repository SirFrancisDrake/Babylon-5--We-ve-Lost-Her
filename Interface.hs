
{-# LANGUAGE FlexibleInstances #-}

module Interface where

import Control.Concurrent
import Control.Concurrent.STM
import Data.Char (toLower)
import Data.Maybe (isJust, fromJust)
import Data.IntMap hiding (filter, map)
import System.Console.Readline

import ErrorMessages
import InterfaceShow
import Owners
import Ppr
import ShipsAndStations
import Stock
import StringFunctions
import Wares
import Wrappers
import World

data UserCommand = UCBack
                 | UCBuy
                 | UCCharacterInfo
                 | UCExit
                 | UCGoTo
                 | UCList
                 | UCNavigation
                 | UCStationInfo
                 | UCSell
                 | UCTrade
                 | UCUndock
              -- | UCSave
    deriving (Show, Eq)

data UserCommandArgs = UCANone
                     | UCATrade Ware Amount
                     | UCAGoTo StationID

-- REMINDER FOR CONTEXTUALSHOW, borrowing from InterfaceShow.hs
--
-- class ContextualShow a where
--       contextShow :: InterfaceState -> a -> String
--
-- type InterfaceState = (Context, Screen)
--
-- data Context = ContextSpace
--              | ContextStationGuest StationID
--              | ContextStationOwner StationID
--              | ContextShipGuest ShipID
--     deriving (Eq, Show)
-- 
-- data Screen = ScreenNavigation
--             | ScreenCharacter
--             | ScreenTrade
--             | ScreenMain
--     deriving (Eq, Show)

instance ContextualShow UserCommand where
    contextShow (c, _) UCBack = "back (to main interface screen)"
    contextShow (c, _) UCBuy = "buy (usage: `buy ware amount` or `buy amount ware`)"
    contextShow (c, _) UCCharacterInfo = "char (show character info)"
    contextShow (c, _) UCExit = "exit (the game)"
    contextShow (c, _) UCGoTo = "go (to a station, usage: `go stationID`)"
    contextShow (ContextSpace, ScreenNavigation) UCList = 
        "list (of known stations in the universe)"
    contextShow (c, _) UCList = "list (stock of the station or ship's cargo)"
    contextShow (c, _) UCNavigation = "navigation (go to navigation screen)"
    contextShow (c, _) UCSell = "sell (see buy)"
    contextShow (c, _) UCStationInfo = "info (overview of the station)"
    contextShow (c, _) UCTrade = "trade (go to trade screen)"
    contextShow (c, _) UCUndock = "undock (from the station)"
    contextShowList i@(c, _) ts = "\n\t-> " ++ 
                concatWith "\n\t-> " (map (contextShow i) ts)

instance ContextualShow UserCommandArgs where
    contextShow _ UCANone = ""
    contextShow _ (UCATrade w a) = show w ++ " " ++ show a
    contextShow _ (UCAGoTo s) = show s

instance ContextualShow (UserCommand, UserCommandArgs) where
    contextShow c (uc,uca) = contextShow c uc ++ " " ++ contextShow c uca

instance Recognize Int where
    recognize i = if filter (flip elem "1234567890") i == i then Just (read i)
                                                            else Nothing

frst (a,_,_) = a
scnd (_,b,_) = b
thrd (_,_,c) = c

ucRecognizeData :: [(UserCommand, String, String -> Maybe UserCommand -> Maybe (UserCommand,UserCommandArgs))]
ucRecognizeData =
    [ (UCBack, "back", noArgs)
    , (UCBuy, "buy", recognizeTradeArgs . words)
    , (UCCharacterInfo, "character", noArgs)
    , (UCExit, "exit", noArgs)
    , (UCGoTo, "go", recognizeGoArgs . words)
    , (UCList, "list", noArgs)
    , (UCNavigation, "navigation", noArgs)
    , (UCSell, "sell", recognizeTradeArgs . words)
    , (UCStationInfo, "info", noArgs)
    , (UCTrade, "trade", noArgs)
    , (UCUndock, "undock", noArgs)
    ]

smartRecognize :: String -> [UserCommand] -> Maybe (UserCommand, UserCommandArgs)
smartRecognize str ucs =
    let nucs = filter (\t -> frst t `elem` ucs) ucRecognizeData
        nuc = exhaustive (map toLower str) (map scnd nucs)
    in if isJust nuc then let (a,_,c) = head $ filter (\t -> scnd t == fromJust nuc) nucs
                          in c str (Just a)
                     else Nothing

noArgs :: String -> Maybe UserCommand -> Maybe (UserCommand, UserCommandArgs)
noArgs s j | (tail . words) s == [] = j >>= \i -> return (i,UCANone)
           | otherwise = err_argumentsShouldBeEmpty

recognizeTradeArgs :: [String] -> Maybe UserCommand -> Maybe (UserCommand, UserCommandArgs)
recognizeTradeArgs (_:w:a:[]) muc = do
    let w1 = recognize w :: Maybe Ware
    let a1 = recognize a :: Maybe Amount
    let w2 = recognize a :: Maybe Ware
    let a2 = recognize w :: Maybe Amount
    uc <- muc
    if (isJust w1) && (isJust a1) then Just $ (uc, UCATrade (fromJust w1) (fromJust a1))
                                else if (isJust w2) && (isJust a2) 
                                        then Just $ (uc, UCATrade (fromJust w2) (fromJust a2))
                                        else Nothing
recognizeTradeArgs _ _ = err_recognizeTradeArguments

recognizeGoArgs :: [String] -> Maybe UserCommand -> Maybe (UserCommand, UserCommandArgs)
recognizeGoArgs (g:i:[]) muc = do
    uc <- muc
    if (isJust $ (recognize i :: Maybe Int)) 
        then Just $ (UCGoTo, UCAGoTo (fromJust $ recognize i))
        else Nothing
recognizeGo _ _ = err_recognizeGoArguments

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

getValidCommand :: [UserCommand] -> IO (UserCommand, UserCommandArgs)
getValidCommand ucs = do
    putStr "\n###> "
    comm <- readline "" >>= return . fromJust
    let c = smartRecognize comm ucs
    if (isJust c) then return (fromJust c)
                  else putStrLn "Nah, wrong way, sorry. Try again\n"
                       >> getValidCommand ucs

chooseAvailibleCommands :: InterfaceState -> [UserCommand]
chooseAvailibleCommands (ContextSpace, ScreenNavigation) =
    [ UCCharacterInfo
    , UCExit
    , UCGoTo
    , UCList
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
    , UCBuy
    , UCCharacterInfo
    , UCExit
    , UCList
    , UCSell
    , UCStationInfo
    ]
chooseAvailibleCommands (_, ScreenCharacter) =
    [ UCBack
    , UCExit
    ]
chooseAvailibleCommands _ = undefined

printAvailibleCommands :: [UserCommand] -> InterfaceState -> IO ()
printAvailibleCommands ucs istate = 
    putStrLn $ "You can do something of the following: " ++ (contextShowList istate ucs)

getUserCommand :: InterfaceState -> IO ((UserCommand, UserCommandArgs), Bool)
getUserCommand istate = do
    let cmds = chooseAvailibleCommands istate
    printContext istate
    printAvailibleCommands cmds istate
    cmd <- getValidCommand cmds
    putStrLn $ contextShow istate cmd
    if fst cmd == UCExit then return (cmd, True)
                         else return (cmd, False)

executeUserCommand :: (UserCommand,UserCommandArgs) -> InterfaceState 
                                -> World -> IO (UserResult, InterfaceState)
executeUserCommand (UCStationInfo,_) istate w = do
    stid <- (world_ships w) !!! 0 >>= return . dockedStID
    st <- (world_stations w) !!! stid
    return (URAnswer (contextShow istate st), istate)

executeUserCommand (UCCharacterInfo,_) istate w = do
    o <- (world_owners w) !!! 0
    let nistate = interface_character istate
    return (URAnswer (contextShow nistate o), nistate)

executeUserCommand (UCList,_) istate@(ContextStationGuest stid, ScreenTrade) w = do
   st <- (world_stations w) !!! stid
   return (URAnswer (pprShow $ filterTrading $ station_stock st), istate)

executeUserCommand (UCList,_) istate@(ContextSpace, ScreenNavigation) w = do
    let vals imap = map ((!) imap) (keys imap)
    -- let's see if you're smart enough to digest this one:
    sts <- readTVarIO (world_stations w) >>= \imap -> sequence
           $ zipWith (>>=) (map readTVarIO (vals imap)) 
                           (map (\k -> \a -> return (k,a)) (keys imap))
    let header = "\n\tID   Station Name\n\t" ++ replicate 20 '*' ++ "\n"
    let newPairs = map (\(a,b) -> "\t" ++ show a ++ "    " ++ station_name b ++ "\n") sts
    return (URAnswer (concat $ header:newPairs), istate)

executeUserCommand (UCUndock,_) istate@(ContextStationGuest stid, ScreenNavigation) w = do
   tst <- readTVarIO (world_stations w) >>= \imapst -> return $ imapst ! stid
   shid <- getOwnerShipID w
   tsh <- readTVarIO (world_ships w) >>= \imapsh -> return $ imapsh ! shid
   atomically $ undockShSt tsh shid tst stid
   return (URSuccess, (ContextSpace, ScreenNavigation))

executeUserCommand (UCUndock,_) istate@(ContextStationOwner stid, ScreenNavigation) w = do
   tst <- readTVarIO (world_stations w) >>= \imapst -> return $ imapst ! stid
   shid <- getOwnerShipID w
   tsh <- readTVarIO (world_ships w) >>= \imapsh -> return $ imapsh ! shid
   atomically $ undockShSt tsh shid tst stid
   return (URSuccess, (ContextSpace, ScreenNavigation))

executeUserCommand (UCGoTo, UCAGoTo stid) istate@(ContextSpace, ScreenNavigation) w = do
   getOwnerShipT w >>= \sh -> atomically (setOnCourse sh stid)
   return (URSuccess, (ContextSpace, ScreenNavigation))

executeUserCommand (UCExit,_) istate _ = return (URSuccess, istate)
executeUserCommand (UCBack,_) istate _ = return (URSuccess, interface_back istate)
executeUserCommand (UCTrade,_)  istate@(ContextStationGuest stid, _) w = do
   st <- (world_stations w) !!! stid
   let stStock = "\nStation stock is:\n" ++ (pprShow $ station_stock st)
   o <- getOwner w
   let budget = "\nYour budget is: " ++ (show $ owner_money o) ++ " credits."
   sh <- getOwnerShip w
   let shCargo = "\nYour ship's cargo bay is:\n" ++ (show $ ship_cargo sh)
   return (URAnswer (stStock ++ "\n" ++ budget ++ "\n" ++ shCargo) , interface_trade istate)

executeUserCommand (UCNavigation,_) istate _ = return (URSuccess, interface_navigation istate)

executeUserCommand (UCBuy, UCATrade bw ba) istate@(ContextStationGuest stid, ScreenTrade) w = do
    applyTradeFn canBuy buy stid w bw ba >>= \r -> return (r, istate)

executeUserCommand (UCSell, UCATrade sw sa) istate@(ContextStationGuest stid, ScreenTrade) w = do
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

getOwnerShipT :: World -> IO (TVar Ship)
getOwnerShipT w = readTVarIO (world_ships w) >>= \shs -> return $ shs ! 0
                                                     -- FIXME when needed

getOwnerShipID :: World -> IO ShipID
getOwnerShipID w = return 0 -- FIXME when needed

getOwner :: World -> IO Owner
getOwner w = (world_owners w) !!! 0 -- FIXME when needed

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

-- DEPRECATED, DELETE IF NO LONGER LOOKS INSPIRING
-- deviseContext :: World -> IO Context
-- deviseContext w = do
--     ownerShip <- getOwnerShip w
--     let nm = ship_navModule ownerShip
--     let np = navModule_position nm
--     case np of
--         (DockedToStation stid) -> return $ ContextStationGuest stid
--         (DockedToShip shid) -> return $ ContextShipGuest shid
--         (SNPSpace pos) -> return $ ContextSpace
    
interfaceCycle :: World -> InterfaceState -> IO ()
interfaceCycle world istate = do
    showSituation world istate
    (command, ifStop) <- getUserCommand istate
    (result, newIstate) <- executeUserCommand command istate world
    showResult result
    putStrLn ""
    if not ifStop then interfaceCycle world newIstate
                  else return ()

pause :: MVar () -> IO ()
pause = takeMVar

unpause :: MVar () -> IO ()
unpause = flip putMVar ()
