module Interface where

import Data.Char (toLower)
import Data.Maybe (isJust, fromJust)

import Stations
import StringFunctions
import Wares
import Wrappers
import World

data UserCommand = UCGoTo StationID
                 | UCSell Ware Amount
                 | UCBuy Ware Amount
                 | UCUndock
                 | UCListCargo
              -- | UCSave
                 | UCExit
    deriving ()

instance Show UserCommand where
    show (UCGoTo i) = "goto station " ++ show i
    show (UCSell w a) = "sell " ++ show a ++ " units of " ++ show w
    show (UCBuy w a) = "buy " ++ show a ++ " units of " ++ show w
    show UCUndock = "undock"
    show UCListCargo = "list station cargo"
    show UCExit = "exit"

instance Eq UserCommand where
    (==) (UCGoTo _) (UCGoTo _) = True
    (==) (UCSell _ _) (UCSell _ _) = True
    (==) (UCBuy _ _) (UCBuy _ _) = True
    (==) UCUndock UCUndock = True
    (==) UCListCargo UCListCargo = True
    (==) UCExit UCExit = True
    (==) _ _ = False

instance Recognize Int where
    recognize i = if filter (flip elem "1234567890") i == i then Just (read i)
                                                            else Nothing

instance Recognize UserCommand where
    recognize uc = let patterns = ["go", "sell", "buy", "undock", "exit", "quit", "list"]
                   in case exhaustive (map toLower uc) patterns of
                       Just "go" -> recognizeGo $ words uc
                       Just "sell" -> recognizeSell $ words uc
                       Just "buy" -> recognizeBuy $ words uc
                       Just "undock" -> Just UCUndock
                       Just "exit" -> Just UCExit
                       Just "quit" -> Just UCExit
                       Just "list" -> Just UCListCargo
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

data Context = Space
             | ContextStation Station
    deriving (Eq,Show)

type UserResult = String -- FIXME

getValidCommand :: [UserCommand] -> IO UserCommand
getValidCommand ucs = do
    putStrLn $ "Please, enter a command. It could be one of the following: \n\t\t" ++ show ucs 
    comm <- getLine
    let c = recognize comm
    if (isJust c) && (fromJust c) `elem` ucs 
        then return (fromJust c)
        else putStrLn "Nah, wrong way, sorry. Try again\n"
             >> getValidCommand ucs

getUserCommand :: Context -> IO (UserCommand, Bool)
getUserCommand Space = putStrLn "You're in open space, cheers" >> return (UCExit, True)
getUserCommand (ContextStation st) = undefined

executeUserCommand :: UserCommand -> World -> IO (UserResult)
executeUserCommand _ _ = undefined

showResult :: UserResult -> IO ()
showResult _ = undefined

showSituation :: World -> IO ()
showSituation w = undefined

deviseContext :: World -> IO Context
deviseContext = undefined
    
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
