module StringFunctions where

import Data.List (inits, intersperse)
import Control.Monad (join)

class Recognize a where
    recognize :: String -> Maybe a

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
exhaustive pattern patterns = let patternList = filter (\pt -> (head $ words pattern) `elem` (inits pt)) patterns
                              in if (length patternList == 1) then Just $ head patternList
                                                              else Nothing

concatWithSpaces :: [String] -> String
concatWithSpaces ss = join $ intersperse " " ss
