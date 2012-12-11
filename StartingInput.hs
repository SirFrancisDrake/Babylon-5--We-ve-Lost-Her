module StartingInput (
    debStartingInput
  , getStartingInput
  , StartingInput
  , Career
  , Race)
  where

import Control.Applicative ((<$>))
import Data.Char (isAlpha, isSpace, toLower)
import Data.Maybe (fromJust, isJust)
import System.Console.Readline

import Auxiliary.StringFunctions
import AI
import DataTypes
import PersonalData
import ShipsAndStations
import ShipsData
import Owner
import VariousFns

debStartingInput :: IO StartingInput -- FIXME remove this debug fn when Interface is fully tested
debStartingInput = return $ genStartingInput "Delen" Minbari Merchant "Liandra"

getStartingInput :: IO StartingInput
getStartingInput = do putStrLn "Greetings. You're about to play Babylon 5: We've Lost Her\n"
                      putStr "Please, type in the name of your character: "
                      charName <- getValidInput get_charName
                      putStr "Please, choose your race. The choice is: Human, Minbari or Narn: "
                      race <- getValidInput get_race
                      putStr "Please, choose your starting career. It could be military or merchant: "
                      career <- getValidInput get_career
                      putStr "Now, come up with the name of your starting ship: "
                      shipName <- getValidInput get_shipName
                      putStrLn $ "\nThank you, the character generation seems to be over. You are a " ++ show race ++
                                " " ++ show career ++ " named " ++ show charName ++ "." ++ " You own a ship named " ++
                                show shipName ++ "."
                      return $ genStartingInput charName race career shipName

genStartingInput :: CharName -> Race -> Career -> ShipName -> StartingInput
genStartingInput name race career shipName = ( (genOwner name race career)
                                             , (genShip shipName race career)
                                             )

genOwner :: CharName -> Race -> Career -> Owner
genOwner name race career = Owner name
                                  [] -- stations owned
                                  [] -- ships owned
                                  (Person race career) 
                                  (startingMoney race career)

fetchShip :: Race -> Career -> Ship
fetchShip Human Merchant = shStandardRhino
fetchShip Human Military = shStandardClark
fetchShip Minbari Merchant = shStandardLiandra
fetchShip Minbari Military = shStandardHel
fetchShip Narn Merchant = shStandardGQuan
fetchShip Narn Military = shStandardLondo

genShip :: ShipName -> Race -> Career -> Ship
genShip n r c = (fetchShip r c){ ship_name = n
                               , ship_AI = SAIPlayer
                               }
                               

startingMoney :: Race -> Career -> Int
startingMoney Human Merchant = 2000
startingMoney Human Military = 1400
startingMoney Minbari Merchant = 2000
startingMoney Minbari Military = 1100
startingMoney Narn Merchant = 1200
startingMoney Narn Military = 400

type CharName = String
type ShipName = String

type StartingInput = (Owner,Ship)

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
                    
getValidInput :: (Eq a) => (String -> Maybe a) -> IO a
getValidInput fn = do
    input <- fn <$> (readline "" >>= return . fromJust)
    if isJust input then return $ fromJust input
                    else putStr "You seem to be a turtle. Try again: " >> 
                         getValidInput fn

validName :: String -> Bool
validName s = flip matchAllPredicates s
    [ mapMatchAnyPredicates [isAlpha -- all symbols are either letters
                            , isSpace -- , or spaces
                            , flip elem "-'"] -- or dashes and apostrophes
    , \t -> length (words t) <= 3 -- no more than 3 words
    ] -- FIXME for not too many non-letter symbols and first letters capitalized 

get_charName :: String -> Maybe CharName
get_charName s = if validName s then Just s
                                else Nothing

get_shipName :: String -> Maybe ShipName
get_shipName s = if validName s then Just s
                                else Nothing

get_race :: String -> Maybe Race
get_race = recognize

get_career :: String -> Maybe Career
get_career = recognize
