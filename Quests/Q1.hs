module Quests.Q1 where

import Control.Applicative ((<$>))
import Data.List (intersperse)
import Data.Maybe (fromJust)
import System.Console.Readline
import Text.ParserCombinators.Parsec

import Auxiliary.Zipper

data Quest = Quest { q_title :: String
                   , q_screens :: Zipper Screen
                   }

data Screen = Screen { s_id :: Int
                     , s_descr :: String
                     , s_actions :: [Action]
                     }

data Action = Action
    { a_descrs :: [String]
    , a_check :: GVs -> Bool
    , a_screenT :: Int
    , a_screenF :: Int
    }

selectScreen :: Quest -> Int -> Quest
selectScreen q i =
    let z = q_screens q
    in q{ q_screens = selectBy z (\a -> s_id a == i) }

type GVs = [ (String, Int) ]

check :: GVs -> String -> (Int -> Bool) -> Bool
check gvs id fn =
    fn (snd $ head $ (filter (\a -> fst a == id) gvs))

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

runQ :: Quest -> GVs -> IO ()
runQ q gvs = 
    if ( s_id $ zip_current $ q_screens q) == 0 
        then return ()
        else do
                putStrLn $ (++"\n") $ s_descr $ zip_current $ q_screens q
                action <- getAction (s_actions $ zip_current $ q_screens q)
                if (a_check action) gvs
                    then runQ (selectScreen q (a_screenT action)) gvs
                    else runQ (selectScreen q (a_screenF action)) gvs
