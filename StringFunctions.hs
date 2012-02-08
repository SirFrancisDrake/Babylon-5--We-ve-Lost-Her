module StringFunctions where

import Data.List (inits)

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

exhaustive :: (Eq a) => [a] -> [[a]] -> Maybe [a]
exhaustive pattern patterns = let patternList = filter (\pt -> pattern `elem` (inits pt)) patterns
                              in if (length patternList == 1) then Just $ head patternList
                                                              else Nothing
