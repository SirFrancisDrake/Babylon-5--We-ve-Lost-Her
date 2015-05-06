
module Data.ProductionChains 
( makeProductions
) where

import DataTypes

import Data.List ((\\), intersperse)
import Text.ParserCombinators.Parsec

type Chain = [ ([(Ware, Amount)], [(Ware, Amount)]) ]

a = "30 Copper and 10 Tin into 5 Bronze, 5 Bronze into 1 Sword"
b = "5 Grain and 1 Salt into 1 Bread"
c = "30 Copper and 10 Tin into 5 Bronze, 5 Bronze and 3 Wood into 1 Axe"

chs = [a, b, c]

chains = map parseChain chs

makeProductions :: [Production]
makeProductions = 
  let chs = concat $ map 
                      (map (\(is, os) -> Production undefined undefined is os 1 [])) 
                      chains
      allIns  = concat $ foldl (\acc p -> (production_inputs  p):acc) [] chs
      allOuts = concat $ foldl (\acc p -> (production_outputs p):acc) [] chs
      leaves  = map (\p -> Production undefined undefined [] [p] 1 []) (allIns \\ allOuts)
  in chs ++ leaves
                      

parseChain :: String -> Chain
parseChain s = 
  case parse chainP "" s of
    Left _  -> error $ "ParseChain: Can't parse " ++ s ++ "\n"
    Right c -> c

chainP = sepBy productionP (try $ string ", ")

--productionP :: Parser ( [ (Ware, Amount), (Ware,Amount) ] )
productionP = do
  ins  <- pairsP
  string " into "
  outs <- pairsP
  return (ins, outs)

pairsP :: Parser [ (Ware, Amount) ]
pairsP = do
  sepBy pairP (try $ string " and ")

pairP :: Parser (Ware, Amount)
pairP = do
  i <- many1 digit
  char ' '
  w <- wareP
  return (w, read i)

wareP :: Parser Ware
wareP =
  let wps = map (\w -> (w, show w)) [minBound..maxBound]
      ps  = map (\(w,s) -> try (string s) >> return w) wps
  in  foldl (<|>) (head ps) (tail ps)
