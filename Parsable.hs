
module Parsable
( Parsable
) where

import qualified Data.Char as Ch (toLower)
import Data.List (foldl)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Combinator

import Wares

toLower :: String -> String
toLower = map Ch.toLower

class Parsable a where -- minimal sufficient declaration: getParser
  getParser  ::  a  -> Parser a
  getParsed  ::  a  -> String -> Maybe a
  parseAnyOf :: [a] -> String -> Maybe a

  getParsed q input =
    let inp = toLower input
    in case parse (getParser q) "" inp of
         Left err -> Nothing
         Right k -> Just k

  parseAnyOf ts input =
    let inp = toLower input
        parserList = map (try . getParser) ts
        bigParser = foldl (<|>) (head parserList) (tail parserList)
    in case parse bigParser "" inp of
         Left err -> Nothing
         Right k -> Just k

-- parsable declarations are here, they're all pretty straightforward

instance Parsable Ware where
  getParser w = (string $ toLower $ show w) >> return w
