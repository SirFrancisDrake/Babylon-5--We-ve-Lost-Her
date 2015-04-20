
{-# LANGUAGE PatternGuards #-}

module WorldGenerator where

import Control.Applicative ((<$>))
import Control.Concurrent.STM
import Control.Monad (filterM, foldM, when)
import Data.List (foldl')

import Data.Productions
import Data.Provinces
import Data.StoringTypes
import DataTypes

makeWorld :: STM World
makeWorld = do
  agents <- makeAgents []
  provinces <- mapM makeProvince startingProvinces
  productions <- mapM (flip makeProduction provinces) startingProductions
  contracts <- setUpContracts productions
  return (World provinces productions agents contracts)

makeAgents :: [Agent] -> STM [TVar Agent]
makeAgents = mapM newTVar

makeProvince :: Province -> STM (TVar Province)
makeProvince = newTVar

makeProduction :: (String, Production) -> [TVar Province] -> STM (TVar Production)
makeProduction (provn, prod) tprovs = do
  tprov <- head <$> filterM (\tp -> readTVar tp >>= return . (== provn) . province_name) tprovs
  prov <- readTVar tprov
  tprod <- newTVar prod{ production_province = tprov }
  writeTVar tprov prov{ province_productions = tprod:(province_productions prov) }
  return tprod

setUpContracts :: [TVar Production] -> STM [TVar Contract]
setUpContracts tps = foldM (\acc tp -> setUpContracts1 tp tps >>= return . (acc++)) [] tps

setUpContracts1 :: TVar Production -> [TVar Production] -> STM [TVar Contract]
setUpContracts1 tprod tprods = do -- that modifies tprods, mind the special effects
  ps <- mapM readTVar tprods
  p <- readTVar tprod
  cs <- canSupply p
  case cs of
    Nothing -> return []
    Just (w,a) -> do
      demp <- filterM  (\(_,p) -> canDemandP p w) (zip tprods ps)
      pairs <- mapM (\(tp,p) -> canDemandN p w >>= return . ((,) tp)) demp
      let sel = generateSelectionByNumber a (map snd pairs)
      let npairs = zip (map fst pairs) sel
      let ctrs = map (\(tp,a) -> makeContractFromSelection tprod tp w a) npairs
      tctrs <- mapM newTVar ctrs
      mapM_ addContractToProductions tctrs
      return tctrs

makeContractFromSelection :: TVar Production -> TVar Production -> Ware -> Amount -> Contract
makeContractFromSelection tp1 tp2 w a =
  Contract 
    CS_Outstanding
    CL_Forever
    (ContractRoute tp1 tp2 w a)

generateSelectionByNumber :: Int -> [Int] -> [Int]
generateSelectionByNumber n ns = takeWhile (/= 0) $ snd $
  foldl' (\(rem, acc) x -> if rem <= x then (0, acc ++ [rem]) 
                                       else (rem - x, acc ++ [x])) 
         (n,[]) 
         ns

canSupply :: Production -> STM (Maybe (Ware, Amount))
canSupply p@Production { production_outputs   = outs
                       , production_contracts = tctrs }
  = do let m (w, _) = do ctrs <- mapM readTVar tctrs
                         let ps = map (\c -> ( contractRoute_ware   $ contract_route c
                                             , contractRoute_amount $ contract_route c ))
                                      ctrs
                         return $ sum $ map snd $ filter (\(w1, a1) -> w1 == w) ps
       t <- filterM (\o@(_,a) -> m o >>= return . (==a)) outs
       let t1 = foldr (\(w,a) all -> map (\p@(w1,a1) -> if w1 == w then (w, a1-a) else p) 
                                     all) outs t
       if null t1
         then return Nothing
         else return $ Just $ head t1

canDemandN :: Production -> Ware -> STM Int
canDemandN p w 
  | null (filter (\(a,_) -> a == w) (production_inputs p)) = return 0
  | otherwise = do
  let demand = snd $ head $ filter (\(a,_) -> a == w) (production_inputs p)
  ctrs <- mapM readTVar (production_contracts p)
  let filleddemand = sum $ map contractRoute_amount 
                               (filter (\cr -> (contractRoute_ware cr) == w) 
                                       (map contract_route ctrs))
  return $ demand - filleddemand

canDemandNP :: Production -> Ware -> Amount -> STM Bool
canDemandNP p w a = canDemandN p w >>= return . (a<=)

canDemandP :: Production -> Ware -> STM Bool
canDemandP p w    = canDemandN p w >>= return . (0<) 

addContractToProductions :: TVar Contract -> STM ()
addContractToProductions tc = do
  c <- readTVar tc
  let tp1 = contractRoute_from $ contract_route c
  let tp2 = contractRoute_to   $ contract_route c
  let modprod tp = do p <- readTVar tp
                      writeTVar tp p{ production_contracts = 
                                      tc:(production_contracts p) }
  modprod tp1
  modprod tp2
