module GenericInterface where

import Data.Function (on)

type UserCommand = State
data MatchType = Exact
               | Exhaustive
    deriving (Eq)

type Pattern = String
type Context = String

type State = String
data TransitionFn = TransitionFn
  { tfn_userCommand :: UserCommand
  , tfn_pattern :: Pattern
  , tfn_matchType :: MatchType
  , tfn_recognizeArgs :: String -- FIXME
  , tfn_contextChange :: Context -> Context
  , tfn_newState :: State
  }

data Zipper a = Zipper 
    { zip_current :: a 
    , zip_other :: [a]
    }
    deriving (Eq, Show)

type FullState = (State, [TransitionFn])

data FiniteAutomata = FiniteAutomata
  { fa_fullStates :: [FullState]
  , fa_startingState :: FullState
  , fa_terminalStates :: [State]
  }

data FiniteAutomata = FiniteAutomata
  { fa_states :: Zipper State 
  , fa_transitionFns :: [TransitionFn]
  , fa_startingState :: State
  , fa_terminalStates :: [State]
  }

instance Eq TransitionFn where
    (==) = on (==) tfn_userCommand

finiteAutomataCycle :: FiniteAutomata -> IO ()
finiteAutomataCycle fa@(FiniteAutomata st tfns stSts termSts) =
 do validInput <- getInput tfns
    newFa <- applyTfn validInput 
    ...

getInput :: FiniteAutomata -> IO TransitionFn
getInput fa =
let patterns = map tfn_pattern 

thrd (_,_,c) = c

check :: FiniteAutomata -> IO ()
check (FiniteAutomata sts tfns stSts termSts) =
    let integralT = map (\t -> (tfn_userCommand t, tfn_newState t, tfn_newState t `elem` sts))
        transitionIntegrity = and (map thrd (integralT tfns))
        failedTransitionIntegrity = mapM_ (\(s,ns,_) -> putStrLn $
                                            "In command " ++ show s ++
                                            ", target state " ++ show ns ++
                                            " does not exist.")
                                          (filter (\t -> not $ thrd t) (integralT tfns))
        passedTransitionIntegrity = "Transitional integrity passed. "
        startingIntegrity = stSts `elem` sts
        termIntegrity = map (flip elem sts) termSts
    in do
        putStrLn "\nVerifying if transition states exist as states."
        if transitionIntegrity then putStrLn "Transition states integrity test passed."
                               else failedTransitionIntegrity
        putStrLn "\nChecking if starting state is a state."
        if startingIntegrity then putStrLn "Passed."
                             else putStrLn "Failed."
        putStrLn "\nChecking if terminal states are states."
        if and termIntegrity then putStrLn "Passed."
                             else putStrLn $ "Those ones failed: " ++ show (filter (flip notElem sts) termSts)
