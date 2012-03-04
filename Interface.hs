module Interface where

import Control.Concurrent.STM
import Data.Char (toLower)
import Data.List (sort)
import Data.Maybe (isJust, fromJust)
import Data.IntMap hiding (filter, map)

import Ships
import Stations
import StringFunctions
import Wares
import Wrappers
import World

data UserCommand = UCGoTo StationID
                 | UCSell Ware Amount
                 | UCBuy Ware Amount
                 | UCStationInfo
                 | UCCharacterInfo
                 | UCUndock
                 | UCList
              -- | UCSave
                 | UCExit
    deriving ()

instance Show UserCommand where
    show (UCGoTo i) = "goto station " ++ show i
    show (UCSell w a) = "sell " ++ show a ++ " units of " ++ show w
    show (UCBuy w a) = "buy " ++ show a ++ " units of " ++ show w
    show UCUndock = "undock"
    show UCList = "list station cargo"
    show UCExit = "exit"

class ContextualShow a where
    contextShow :: Context -> a -> String
    contextShowList :: Context -> [a] -> String
    contextShowList c = concatWithSpaces . sort . map (contextShow c)

instance ContextualShow UserCommand where
    contextShow c (UCGoTo i) = "go"
    contextShow c (UCSell w a) = "sell"
    contextShow c (UCBuy w a) = "buy"
    contextShow c UCUndock = "undock"
    contextShow c UCList = "list"
    contextShow c UCExit = "exit"
    contextShow c UCStationInfo = "info"
    contextShow c UCCharacterInfo = "char"

instance Eq UserCommand where
    (==) (UCGoTo _) (UCGoTo _) = True
    (==) (UCSell _ _) (UCSell _ _) = True
    (==) (UCBuy _ _) (UCBuy _ _) = True
    (==) UCUndock UCUndock = True
    (==) UCList UCList = True
    (==) UCExit UCExit = True
    (==) _ _ = False

instance Recognize Int where
    recognize i = if filter (flip elem "1234567890") i == i then Just (read i)
                                                            else Nothing

instance Recognize UserCommand where
    recognize uc = let patterns = ["go", "sell", "buy", "undock", "exit", "quit", "list", "char", "info"]
                   in case exhaustive (map toLower uc) patterns of
                       Just "go" -> recognizeGo $ words uc
                       Just "sell" -> recognizeSell $ words uc
                       Just "buy" -> recognizeBuy $ words uc
                       Just "undock" -> Just UCUndock
                       Just "exit" -> Just UCExit
                       Just "quit" -> Just UCExit
                       Just "list" -> Just UCList
                       Just "info" -> Just UCStationInfo
                       Just "char" -> Just UCCharacterInfo
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

data Context = ContextSpace
             | ContextStation StationID
             | ContextShip ShipID
    deriving (Eq,Show)

printContext :: Context -> IO ()
printContext ContextSpace = putStrLn "You're in open space."
printContext (ContextStation stid) = putStrLn $ "You're on a station with ID " ++ show stid -- FIXME
printContext (ContextShip shid) = putStrLn $ "You're on a ship with ID " ++ show shid -- FIXME

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
    comm <- getLine
    let c = recognize comm
    if (isJust c) && (fromJust c) `elem` ucs 
        then return (fromJust c)
        else putStrLn "Nah, wrong way, sorry. Try again\n"
             >> getValidCommand ucs

chooseAvailibleCommands :: Context -> [UserCommand]
chooseAvailibleCommands ContextSpace =
    [ UCGoTo 0
    , UCList
    , UCExit
    , UCCharacterInfo
    ]
chooseAvailibleCommands (ContextStation st) =
    [ UCUndock
    , UCBuy Fuel 1
    , UCSell Fuel 1
    , UCList
    , UCStationInfo
    , UCCharacterInfo
    , UCExit
    ]
chooseAvailibleCommands (ContextShip sh) =
    [ UCUndock
    , UCExit
    ]

printAvailibleCommands :: [UserCommand] -> Context -> IO ()
printAvailibleCommands ucs c = 
    putStrLn $ "You can do something of the following: " ++ (contextShowList c ucs)

getUserCommand :: Context -> IO (UserCommand, Bool)
getUserCommand c = do
    let cmds = chooseAvailibleCommands c
    printContext c
    printAvailibleCommands cmds c
    cmd <- getValidCommand cmds
    if cmd == UCExit then return (cmd, True)
                     else return (cmd, False)

executeUserCommand :: UserCommand -> World -> IO (UserResult)
executeUserCommand UCStationInfo w = do
    let ships = world_ships w    
    stid <- readTVarIO ships >>= \i -> readTVarIO (i ! 0) >>= \j ->
       return $ dockedStID j 
    let stations = world_stations w
    st <- readTVarIO stations >>= \i -> readTVarIO (i ! stid)
    return $ URAnswer (station_guestShow st)
executeUserCommand _ _ = undefined

showResult :: UserResult -> IO ()
showResult = print

showSituation :: World -> IO ()
showSituation w = do
    ownerShip <- readTVarIO (world_ships w) >>= \i -> return (i ! 0) >>= readTVarIO
    let nm = ship_navModule ownerShip
    let np = navModule_position nm
    case np of
        (DockedToStation stid) -> readTVarIO (world_stations w) >>= \i ->
                                    return (i ! stid) >>=
                                    readTVarIO >>= putStrLn . station_guestShow
        (DockedToShip shid) -> readTVarIO (world_stations w) >>= \i ->
                                   return (i ! shid) >>= readTVarIO >>=
                                   print
        (SNPSpace pos) -> print pos

deviseContext :: World -> IO Context
deviseContext w = do
    ownerShip <- readTVarIO (world_ships w) >>= \i -> return (i ! 0) >>= readTVarIO
    let nm = ship_navModule ownerShip
    let np = navModule_position nm
    case np of
        (DockedToStation stid) -> return $ ContextStation stid
        (DockedToShip shid) -> return $ ContextShip shid
        (SNPSpace pos) -> return $ ContextSpace
    
interfaceCycle :: World -> IO ()
interfaceCycle world = do
    showSituation world
    context <- deviseContext world
    (command, ifStop) <- getUserCommand context
    result <- executeUserCommand command world
    showResult result
    showSituation world
    if not ifStop then interfaceCycle world
                  else return ()
