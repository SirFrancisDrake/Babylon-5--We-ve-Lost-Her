
module Parse where

import Text.ParserCombinators.Parsec

import Q1

questP :: Parser Quest
questP = do
    title <- titleP
    many (noneOf "\n") >> char '\n'
    screens <- sepBy1 screenP (char '\n')
    return $ Quest title (Zipper (head screens) (tail screens))

titleP :: Parser String
titleP = string "title: " >> many (noneOf "\n")

screenP :: Parser Screen
screenP = do
    id <- intP
    string ". Description: "
    descr <- many (noneOf "\n")
    char '\n'
    string "Actions:\n"
    acts <- many1 actionP
    return $ Screen (read id) descr acts

actionP :: Parser Action
actionP = do
    string " - "
    

intP = many1 (oneOf "1234567890")
