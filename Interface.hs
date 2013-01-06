
{-# LANGUAGE FlexibleInstances #-}

module Interface where

import qualified Data.Map as M
import Text.ParserCombinators.Parsec hiding (getInput)

import Auxiliary.Parsec
import DataTypes
import Parsable
import Wares
import Wrappers

-- interfaceCycle = do
--   (command, terminate) <- getUserCommand
--   executeUserCommand command
--   showResult result
--   if not terminate 
--     then interfaceCycle
--     else return ()
-- 
-- type InputChoice = IChoice IType
-- 
-- data IChoice =
--   IC_Precise [String]
--   | IC_Parsable [String] [ParsableType]
-- 
-- data IType = ICh_Hidden | ICh_Verbose
-- 
-- getInput :: [InputChoice] -> IO InputChoice
-- getInput ics = do
--   input <- getLine
--   let parsedList = map (flip parseChoice input) ics
--   if (length parsedList == 1) 
--     then head parsedList
--     else putStrLn "Nope"
--          getInput ics

askPolitely :: String -> Parser a -> IO a
askPolitely question p = do
  putStrLn question
  let tryOnce = getLine >>= \ans ->
             case parse p "" ans of
               Left err -> putStrLn "Failed to parse your input. Try again" >> tryOnce
               Right a -> return a
  tryOnce

data TradeAction =
  Buy TradeArgs | Sell TradeArgs
  deriving (Show)

type TradeArgs = (Ware, Amount)

data OptionsAction =
  RenameShip String
  | DoNothing
  deriving (Show)

parserWA =
  (try $ do
    w <- getBigParser allWares
    char ' '
    a <- parserAmount
    return (w, a)
  ) <|> (
   try $ do
    a <- parserAmount
    char ' '
    w <- getBigParser allWares
    return (w, a)
  )

parserBuy = do
    string "buy "
    q <- parserWA
    return $ Buy q

parserSell = do
    string "sell "
    q <- parserWA
    return $ Sell q

instance Parsable TradeAction where
  getParser (Buy _)  = parserBuy
  getParser (Sell _) = parserSell

allTradeActions = [Buy undefined, Sell undefined]
