
module Trace where

import Control.Concurrent.STM
import Data.List (intersperse)

import DataTypes

printProduction :: Production -> IO ()
printProduction (Production tprov ins outs ticks tcs) = do
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
