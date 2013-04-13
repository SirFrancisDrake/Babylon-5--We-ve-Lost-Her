module Auxiliary.StringFunctions where

import Control.Monad (join)
import Data.List (inits, intersperse)
import Data.Maybe (isJust, fromJust)
import Text.ParserCombinators.Parsec hiding (parse)
import qualified Text.ParserCombinators.Parsec as P (parse)

class Recognize a where
  recognize :: String -> Maybe a

exhaustiveParse :: [Parser a] -> String -> Maybe a
exhaustiveParse ps str =
  let parseResults = map (\p -> P.parse p "" str) ps
      filterFn either = case either of
                          Left _  -> Nothing
                          Right a -> Just a
      filtered = filter isJust (map filterFn parseResults)
  in if length filtered == 1
       then head filtered
       else Nothing

-- EXAMPLE Recognize INSTANCE DECLARATION
----------------------------------------
-- instance Recognize Career where
--     recognize c = let patterns = ["military", "merchant"]
--                   in case exhaustive (map toLower c) patterns of
--                          Just "military" -> Just Military
--                          Just "merchant" -> Just Merchant
--                          otherwise -> Nothing

exhaustive :: String -> [String] -> Maybe String
exhaustive "" _ = Nothing
exhaustive input patterns = 
  let firstWordOfInput = head $ words input
      patternList = filter (\pt -> firstWordOfInput `elem` (inits pt)) patterns
  in if (length patternList == 1) 
       then Just $ head patternList
       else Nothing

concatWithSpaces :: [String] -> String
concatWithSpaces ss = concatWith " " ss

concatWith :: String -> [String] -> String
concatWith i ss = join $ intersperse i ss
