module Interface where

import Data.Char (toLower)

import Stations
import StringFunctions
import Wares
import Wrappers
import World

data UserCommand = UCGoTo StationID
                 | UCSell Ware Amount
                 | UCBuy Ware Amount
                 | UCUndock
              -- | UCSave
                 | UCExit
    deriving (Show)

instance Recognize Int where
    recognize i = if filter (flip elem "1234567890") i == i then Just (read i)
                                                            else Nothing

instance Recognize UserCommand where
    recognize uc = let patterns = ["go", "sell", "buy", "undock", "exit"]
                   in case exhaustive (map toLower uc) patterns of
                       Just "go" -> recognizeGo uc
                       Just "sell" -> recognizeSell $ words uc

recognizeGo = undefined 

recognizeSell :: [String] -> Maybe UserCommand
recognizeSell (s:w:a:[]) = undefined
recognizeSell _ = Nothing

type Context = String -- FIXME
type UserResult = String -- FIXME

getUserCommand :: Context -> IO (UserCommand, Bool)
getUserCommand context = undefined

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
