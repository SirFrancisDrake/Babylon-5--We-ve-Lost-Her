
{-# LANGUAGE PatternGuards #-}

module Trace where

import Control.Concurrent.STM
import Data.List (intersperse)

import Data.ProductionChains
import DataTypes

printAgent :: Agent -> IO ()
printAgent a = do
  putStrLn $ "\nCapacity: " ++ show (agent_capacity a)
  printState $ agent_state a

printState as
  | AS_Idle <- as = putStr "Idle"
  | AS_LookingForContract <- as = putStr "LookingForContract"
  | AS_WorkingContract tc <- as = putStr "Working contract: " >> readTVarIO tc >>= printContract

printProduction :: Production -> IO ()
printProduction (Production _ tprov ins outs ticks tcs) = do
    putStrLn $ "\n--> Inputs: "  ++ (concat $ intersperse " " $ map show ins)
    putStrLn $ "--> Outputs: " ++ (concat $ intersperse " " $ map show outs)
    ctrs <- mapM readTVarIO tcs
    mapM_ printContract ctrs

printContract :: Contract -> IO ()
printContract (Contract _ _ (ContractRoute tfp ttp w a)) = do
  putStr "\n From: " 
  printProductionProvince tfp
  putStr "\n To: "  
  printProductionProvince ttp
  putStr $ "\n  -> " ++ show a ++ " of " ++ show w ++ "\n"

printProductionProvince :: TVar Production -> IO ()
printProductionProvince tprod =
  readTVarIO tprod >>= readTVarIO .  production_province >>= 
  putStr . province_name
