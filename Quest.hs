
-- qs = QuestState "DR1" :: QuestStateName
--                 "You're in a dark room etc." :: StateDescr
--                 [ ( "climb" :: Command
--                   , () :: Vars -> (Answer, NewState)
--                   , Verbose | Hidden :: CommandType
--                   )
--                 ]
-- 
-- takeAction ::
-- case parse (string a) "" input of
--     a -> trigger
--     b -> trigger
--     ...
--     etc.

import qualified Control.Monad.State as S
import Control.Monad.IO.Class
import Text.ParserCombinators.Parsec hiding (getState)

data Quest = Quest
    { q_name :: String
    , q_states :: [QState]
    , q_context :: VariableContext
    }
    deriving ()

data QState = QState
    { qs_id :: QStateID
    , qs_terminal :: [QStateID]
    , qs_actions :: [ QAction ]
    }
    deriving ()

data QAction = QAction
    { qa_cmd :: Command
    , qa_triggerEx :: TriggerEx
    , qa_triggerWorks :: TriggerWorks
    , qa_cmdType :: CommandType
    , qa_stateChange :: StateChange
    , qa_newState :: QStateID
    }

type VariableContext = [ (QVar, QVal) ]
type QStateID = String
type Command = String
type TriggerEx = VariableContext -> Bool
type TriggerWorks = VariableContext -> Bool
data CommandType = Hidden
                 | Verbose { ctv_short :: String }
    deriving (Eq, Show)
type StateChange = VariableContext -> VariableContext
type QVar = String
type QVal = Int

getAction :: [QAction] -> S.StateT Quest IO QAction
getAction qas = do
    gvs <- S.get >>= return . q_context
    let availActions = filter (\a -> (qa_triggerEx a) gvs) qas
    let mkParser qa =
         if (qa_cmdType qa) == Hidden 
             then string (qa_cmd qa)
             else ( (string (qa_cmd qa))
                   <|> (string (ctv_short $ qa_cmdType qa)) )
    let parsers = foldr (<|>) (head (map mkParser qas)) (tail (map mkParser qas))
    input <- liftIO getLine
    case parse parsers "" input of
        Left err -> error $ "Can't parse action: " ++ (show err)
        Right r -> return (head $ filter (\a -> qa_cmd a == r) qas)

takeAction :: QAction -> S.StateT Quest IO QStateID
takeAction qa = do
    quest <- get
    let context = q_context quest
    if qa_triggerWorks qa
        then 
        else

getState :: QStateID -> State Quest QState
getState qid = do
    qss <- S.get >>= return . q_states
    let qsts = filter (\a -> qs_id a == qid) qss
    if length qsts /= 1 
      then error $ "Wrong number of states named " ++ qid
      else return $ head qsts
    

questCycle :: QState -> S.StateT Quest IO QState
questCycle state = do
    action <- getAction (qs_actions state)
    newState <- takeAction action
    if newState `elem` qs_terminal state
        then return (getState newState)
        else return (getState newState) >>= questCycle

-- questCycle state =
--     context <- get
--     displayState state context
--     (newState,newContext) <- takeAction state context
--     questCycle newState
