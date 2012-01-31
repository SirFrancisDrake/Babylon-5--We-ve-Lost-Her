module BottomTopGame where

import Control.Applicative ( (<$>) )
import Data.Char (toLower)
import Data.List (inits)

import StartingInput ( StartingInput
                     , getStartingInput 
                     )

main = do
    startingInput <- getStartingInput
    resultingInput <- playGame $ makeNewWorld startingInput
    print resultingInput

playGame :: World -> IO Result
playGame world = do newInput <- getCycleInput world
                    case gameCycle world newInput of
                         (Left result) -> return result
                         (Right newWorld) -> playGame newWorld

gameCycle :: World -> CycleInput -> Either Result World
gameCycle = undefined

getCycleInput :: World -> IO CycleInput
getCycleInput = undefined

makeNewWorld :: StartingInput -> World
makeNewWorld = undefined

type World = String
type Station = String
-- A world is: some stations, some ships, player

type Universe = [String]
type StationID = Integer
type Distance = Integer
type JumpRoute = (StationID, StationID, Distance)

modify :: Universe -> (Station -> Bool) -> (Station -> Station) -> Universe
modify u predicate modifier = map (\st -> if predicate st then modifier st else st) u

type CycleInput = String
type Result = String
