
{-# LANGUAGE TypeSynonymInstances #-}

import Data.Function (on)
import Data.List (delete, find)
import Data.Maybe (fromJust, isJust)

import ExtendedTuples

type State = String
type Pattern = String
data TransitionFn = TransitionFn
  { tfn_startingState :: State
  , tfn_pattern :: Pattern
  , tfn_targetState :: State
  }
  deriving (Show)

instance Eq TransitionFn where
    (==) = on (==) tfn_pattern

data Zipper a = Zipper
  { zip_current :: a
  , zip_others :: [a]
  }
  deriving (Eq, Show)

toList :: Zipper a -> [a]
toList (Zipper z zs) = z:zs

select :: (Eq a) => Zipper a -> a -> Zipper a
select (Zipper z zs) t 
  | t `elem` (z:zs) = Zipper t (delete t (z:zs))
  | otherwise = error "Select: non-existent match."

-- data FiniteAutomata = FiniteAutomata
--   { fa_states :: Zipper State
--   , fa_tfns :: [TransitionFn]
--   , fa_terminalStates :: [State]
--   }

type PureFiniteAutomata = (Zipper State, [TransitionFn], [State])

class FiniteAutomata a where
    fa_states :: a -> Zipper State
    fa_tfns :: a -> [TransitionFn]
    fa_terminalStates :: a -> [State]
    fa_setState :: a -> State -> a

instance FiniteAutomata PureFiniteAutomata where
    fa_states = frst
    fa_tfns = scnd
    fa_terminalStates = thrd
    fa_setState (a,b,c) t = (select a t, b, c)

runFA :: (FiniteAutomata a) => [String] -> a -> State
runFA [] fa = zip_current $ fa_states fa
runFA (s:ss) fa = 
    let cstate = (zip_current . fa_states) fa
    in if cstate `elem` fa_terminalStates fa then cstate
                                             else runFA ss (stepFA s fa)

data FARunnerAnswer fa a = FARAError fa
                         | FARAFinished fa
                         | FARAContinue fa a
    deriving (Eq)

instance Monad (FARunnerAnswer fa) where
    return a = FARAContinue a
    (>>=) (FARAError fa) _ = FARAError fa
    (>>=) (FARAFinished fa) _ = FARAFinished fa
    (>>=) (FARAContinue fa a) f = f a

stepFA :: (FiniteAutomata a) => Pattern -> a -> a
stepFA s fa =
    let tfnM = find (\t -> tfn_pattern t == s) (fa_tfns fa)
        tfn = if isJust tfnM then fromJust tfnM
                             else error "Transition fail: can't recognize next symbol"
    in fa_setState fa (tfn_targetState tfn) 

stepFAsmart :: (FiniteAutomata a) => Pattern -> a -> FARunnerAnswer a
stepFAsmart s fa = do
    let tfnM = find ( (== s) . tfn_pattern ) (fa_tfns fa)
        tfn = if isJust tfnM then FARAContinue (fromJust tfnM)
                             else FARAError
        isFinished st = if st `elem` (fa_terminalStates fa) then FARAFinished
                                                            else FARAContinue st
    tfn >>= \t -> (return $ fa_setState fa (tfn_targetState t)) >>= isFinished
