
module Parsable where

import qualified Data.Char as Ch (toLower)
import Data.List (foldl)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Combinator

import Auxiliary.Parsec
import DataTypes
import Wares
import Wrappers

toLower :: String -> String
toLower = map Ch.toLower

class Parsable a where -- minimal sufficient declaration: getParser
  getParser      ::  a  -> Parser a          -- parser for a particular a
  getBigParser   :: [a] -> Parser a          -- parser for any a from given list
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

-- Not in Parsable, because it doesn't need it, nor needed for it
getParsedByNum :: [a] -> String -> Maybe a
getParsedByNum as inp =
  let intParser i = try (string $ show i ++ "\n") >> return i -- Look below for '\n' expl
      parsers = map intParser [1..(length as)]
      bigParser = foldParsers parsers
  in case parse bigParser "" (inp ++ "\n") of
      Left err -> Nothing
      Right k -> Just $ as !! (k - 1)
  -- I'm adding "\n" to both end of input and end of number representation
  -- to make sure parsec doesn't interpret "11" as "1". How do I do that right?

-- takes a number of items and an input string, and:
-- 1) if an input string is an item index, returns the item
-- 2) if an input string matches an item as a string exactly, returns the item
-- 3) returns nothing
getParsedStrNum :: [String] -> String -> Maybe String
getParsedStrNum as inp = -- FIXME remove ``show i ++ "\n"'' redundancy
  let strNums = map (\i -> show i ++ "\n") [1..(length as)]
      pairs = zip strNums as
      numParsers = map (\(i,a) -> string i >> return a) pairs
      strParsers = map (\s -> try (string $ toLower s ++ "\n") >> return s) as
      bigParser = foldParsers
      finalParser = (bigParser numParsers) <|> (bigParser strParsers)
  in case parse finalParser "" (toLower inp ++ "\n") of
    Left err -> Nothing
    Right a -> Just a

switchByStrNum :: [(String, a)] -> String -> Maybe a
switchByStrNum ps inp =
  case getParsedStrNum (map fst ps) inp of
    Nothing -> Nothing
    Just a -> Just $ snd $ head $ (filter (\(b,_) -> b == a) ps)

-- parsable declarations are here, they're all pretty straightforward

instance Parsable Ware where
  getParser w = (string $ toLower $ show w) >> return w

parserAmount :: Parser Amount
parserAmount = many1 digit >>= return . read

instance Parsable Station where
  getParser st = (string $ toLower $ station_name st) >> return st
