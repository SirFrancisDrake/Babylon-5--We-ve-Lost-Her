
module Auxiliary.Concurrent where

import Control.Concurrent

pause :: MVar () -> IO ()
pause a = putStrLn "Pausing. " >> takeMVar a

unpause :: MVar () -> IO ()
unpause a = putStrLn "Unpausing. " >> putMVar a ()

