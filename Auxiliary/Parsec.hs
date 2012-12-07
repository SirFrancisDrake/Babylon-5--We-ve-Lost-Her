
module Auxiliary.Parsec
( foldParsers
, foldParsersE
)
where

import Text.ParserCombinators.Parsec

foldParsers :: [Parser a] -> Parser a
foldParsers [] = error "can't fold 0 parsers"
foldParsers (p:ps) = foldl (<|>) p ps

foldParsersE :: [Parser a] -> String -> Parser a
foldParsersE = (.) (<?>) foldParsers -- same as:
-- foldParsersE :: [Parser a] -> String -> Parser a
-- foldParsersE ps str = (foldParsers ps) <?> str
