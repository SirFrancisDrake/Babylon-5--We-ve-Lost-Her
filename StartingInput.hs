module StartingInput (
    getStartingInput
  , StartingInput
  , Career
  , Race)
  where

import Control.Applicative ((<$>))
import Data.Char (toLower)
import Data.List (inits)
import Data.Maybe (fromJust, isJust)

getStartingInput :: IO StartingInput
getStartingInput = do putStrLn "Greetings. You're about to play Babylon 5: Parting Ways\n"
                      putStr "Please, type in the name of your character: "
                      charName <- getValidInput get_charName
                      putStr "Please, choose your race. The choice is: Human, Minbari or Narn: "
                      race <- getValidInput get_race
                      putStr "Please, choose your starting career. It could be military or merchant: "
                      career <- getValidInput get_career
                      putStrLn $ "\nThank you, the character generation seems to be over. You are a " ++ show race ++
                                " " ++ show career ++ " named " ++ show charName ++ "."
                      return $ StartingInput charName race career

type CharName = String
data StartingInput = StartingInput CharName Race Career

data Race = Human | Minbari | Narn
    deriving (Eq, Show)

data Career = Military | Merchant
    deriving (Eq, Show)

class Recognize a where
    recognize :: String -> Maybe a

instance Recognize Career where
    recognize c = let patterns = ["military", "merchant"]
                  in case exhaustive (map toLower c) patterns of
                         Just "military" -> Just Military
                         Just "merchant" -> Just Merchant
                         otherwise -> Nothing

instance Recognize Race where
    recognize r = let patterns = ["human", "minbari", "narn"]
                  in case exhaustive (map toLower r) patterns of
                         Just "human" -> Just Human
                         Just "minbari" -> Just Minbari
                         Just "narn" -> Just Narn
                         otherwise -> Nothing
                    
exhaustive :: (Eq a) => [a] -> [[a]] -> Maybe [a]
exhaustive pattern patterns = let patternList = filter (\pt -> pattern `elem` (inits pt)) patterns
                              in if (length patternList == 1) then Just $ head patternList
                                                              else Nothing

getValidInput :: (Eq a) => (String -> Maybe a) -> IO a
getValidInput fn = do
    input <- fn <$> getLine
    if isJust input then return $ fromJust input
                    else putStr "You seem to be a turtle. Try again: " >> getValidInput fn

get_charName :: String -> Maybe CharName
get_charName s = if (length s /= 0) then Just s
                                    else Nothing

get_race :: String -> Maybe Race
get_race = recognize

get_career :: String -> Maybe Career
get_career = recognize
