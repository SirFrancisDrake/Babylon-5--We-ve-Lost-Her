
module Parsable where

import qualified Data.Char as Ch (toLower)
import Data.List (foldl)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Combinator

import DataTypes
import Wares
import Wrappers

toLower :: String -> String
toLower = map Ch.toLower

class Parsable a where -- minimal sufficient declaration: getParser
  getParser      ::  a  -> Parser a          -- parser for a particular a
  getBigParser   :: [a] -> Parser a          -- parser for any a from given list
  --getParsedByNum :: [a] -> String -> Maybe a -- doesn't even need getParser
  getParsed      ::  a  -> String -> Maybe a 
  parseAnyOf     :: [a] -> String -> Maybe a

  getParsed q input =
    let inp = toLower input
    in case parse (getParser q) "" inp of
         Left err -> Nothing
         Right k -> Just k

  getBigParser as = 
    let parsers = map (try . getParser) as
    in foldl (<|>) (head parsers) (tail parsers) 

  parseAnyOf ts input =
    let inp = toLower input
    in case parse (getBigParser ts) "" inp of
         Left err -> Nothing
         Right k -> Just k

getParsedByNum as inp =
  let intParser i = try (string $ show i) >> return i -- FIXME parse "1" == parse "11"
      parsers = map intParser [1..(length as)]
      bigParser = foldl (<|>) (head parsers) (tail parsers)
  in case parse bigParser "" inp of
      Left err -> Nothing
      Right k -> Just $ as !! k

-- parsable declarations are here, they're all pretty straightforward

instance Parsable Ware where
  getParser w = (string $ toLower $ show w) >> return w

parserAmount :: Parser Amount
parserAmount = many1 digit >>= return . read

instance Parsable Station where
  getParser st = (string $ toLower $ station_name st) >> return st
