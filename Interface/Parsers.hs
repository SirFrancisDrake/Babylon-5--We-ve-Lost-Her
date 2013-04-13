
module Interface.Parsers where

import Text.ParserCombinators.Parsec

import Interface.Actions
import Parsable
import Wares

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
