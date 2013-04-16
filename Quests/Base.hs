module Quests.Base where

import Control.Applicative ((<$>))
import Control.Concurrent.STM hiding (check)
import qualified Control.Monad.Reader as R
import qualified Control.Monad.State as S
import Data.List (intersperse)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import System.Console.Readline
import Text.ParserCombinators.Parsec

import Auxiliary.Zipper
import Contexts
import DataTypes
import Wrappers

makeQuestContext :: Quest -> QuestVars -> QuestContext
makeQuestContext q qvs =
  QuestContext (q_title q) qvs

checkQVarW :: QScope -> QVarName -> (QVar -> Bool) -> R.ReaderT World STM Bool
checkQVarW qsc qvn fn =
  getPlayerSTM >>= R.lift . readTVar . player_questVars >>= return . (checkP qsc qvn fn)

checkP :: QScope -> QVarName -> (QVar -> Bool) -> QuestVars -> Bool
checkP qsc qvn fn qvars =
  case M.lookup (qsc,qvn) qvars  of
    Just i -> fn i
    Nothing -> True

check :: QScope -> QVarName -> (QVar -> Bool) -> R.Reader QuestContext Bool
check qsc qvn fn =
  R.ask >>= return . (M.lookup (qsc, qvn)) . qc_variables >>= \m ->
  case m of
    Just i -> return $ fn i
    Nothing -> return False

checkLocal :: QVarName -> (QVar -> Bool) -> R.Reader QuestContext Bool
checkLocal qvn fn =
  R.ask >>= \qc -> check (QSLocal $ qc_questName qc) qvn fn

modify :: QScope -> QVarName -> (QVar -> QVar) -> S.State QuestContext ()
modify qsc qvn fn = do
  qcon <- S.get
  let cqvs = qc_variables qcon
  let nm = 
        case M.lookup (qsc,qvn) cqvs of
          Nothing -> cqvs
          Just q -> M.update (return . fn) (qsc,qvn) cqvs
  S.put qcon{ qc_variables = nm }

modifyLocal :: QVarName -> (QVar -> QVar) -> S.State QuestContext ()
modifyLocal qvn fn = S.get >>= return . qc_questName >>= \qn ->
  modify (QSLocal qn) qvn fn

add :: QScope -> QVarName -> QVar -> S.State QuestContext ()
add qsc qvn qvar = do
  qcon <- S.get
  let cqvs = qc_variables qcon
  let nm = 
        case M.lookup (qsc,qvn) cqvs of
          Nothing -> M.insert (qsc,qvn) qvar cqvs
          Just q -> M.update (return . (const qvar)) (qsc,qvn) cqvs
  S.put qcon{ qc_variables = nm }

addLocal :: QVarName -> QVar -> S.State QuestContext ()
addLocal qvn qvar = S.get >>= return . qc_questName >>= \qn ->
  add (QSLocal qn) qvn qvar

selectScreen :: Quest -> Int -> Quest
selectScreen q i =
    let z = q_screens q
    in q{ q_screens = selectBy z (\a -> s_id a == i) }

isRight :: Either a b -> Bool
isRight e = case e of
                Left _ -> False
                Right _ -> True

getAction :: [Action] -> IO Action
getAction as = do
    showActions as
    z <- fromJust <$> readline "#> "
    let descrActionPairs = 
            concatMap (\a -> map (\d -> (a,d) ) (a_descrs a)) as -- FIXME badly written
    let parsers = map (\(a,d) -> (a, string d)) descrActionPairs
    let parsed = map (\(a,p) -> (a, parse p "" z)) parsers
    let action = filter (isRight . snd) parsed
    if length action == 1 then return $ fst $ head action
                          else putStrLn "Can't recognize action.\n" >> getAction as

showActions :: [Action] -> IO ()
showActions as = putStrLn "--> You could:" >>
                 mapM_ (putStrLn . ((++) "  - ") . concat . (intersperse ", ") . a_descrs) as

runQ :: Quest -> S.StateT QuestContext IO ()
runQ q = 
    if ( s_id $ zip_current $ q_screens q) == 0 
        then return ()
        else do
                R.lift $ putStrLn $ (++"\n") $ s_descr $ zip_current $ q_screens q
                action <- R.lift $ getAction (s_actions $ zip_current $ q_screens q)
                S.get >>= return . (R.runReader (a_successCheck action)) >>= \b ->
                  if b
                      then do
                        S.get >>= S.return . (S.runState $ a_modT action) >>= S.put . snd
                        runQ (selectScreen q (a_screenT action))
                      else do
                        S.get >>= S.return . (S.runState $ a_modF action) >>= S.put . snd
                        runQ (selectScreen q (a_screenF action))
