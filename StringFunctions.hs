module StringFunctions where

class Recognize a where
    recognize :: String -> Maybe a
