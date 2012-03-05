module InterfaceShow where

import Data.List (sort)

import StringFunctions
import Wrappers

-- A special Show class for pretty interface printing with relation
-- to the interface context. E.g. a ship would be shown differently depending
-- on whether the player owns the ship, sees it in store and so on.
-- Context ADT represents information about the circumstances in which
-- showing occurs. E.g. we're on a station, or on *our* station etc.
-- Screen ADT represents interface screens, like Trade, Character overview etc.
-- Instance declarations of ContextualShow are located close to respective 
-- ADT definitions.

type InterfaceState = (Context, Screen)

data Context = ContextSpace
             | ContextStationGuest StationID
             | ContextStationOwner StationID
             | ContextShipGuest ShipID
    deriving (Eq, Show)

data Screen = ScreenNavigation
            | ScreenCharacter
            | ScreenTrade
            | ScreenMain
    deriving (Eq, Show)

startingIState = (ContextStationGuest 0, ScreenMain)

interface_back :: InterfaceState -> InterfaceState
interface_back (c, _) = (c, ScreenMain)

interface_trade :: InterfaceState -> InterfaceState
interface_trade (c, _) = (c, ScreenTrade)

interface_character :: InterfaceState -> InterfaceState
interface_character (c, _) = (c, ScreenCharacter)

interface_navigation :: InterfaceState -> InterfaceState
interface_navigation (c, _) = (c, ScreenNavigation)

class ContextualShow a where
    contextShow :: InterfaceState -> a -> String
    contextShowList :: InterfaceState -> [a] -> String
    contextShowList istate = (concatWith "\n\t") . sort . map (contextShow istate)

