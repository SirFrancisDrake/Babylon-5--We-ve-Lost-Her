module Something where

import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.Trans

type World = TVar [Int]

test :: ReaderT World IO ()
test = do
    cylon <- ask
    liftIO $ do
        atomically $ do
            vals <- readTVar cylon
            writeTVar cylon (vals ++ [1,3,5,7])
        putStrLn "Done"

pest :: ReaderT World IO ()
pest = do
    jabba <- ask
    liftIO $ readTVarIO jabba >>= putStrLn . show

main = do
    jabba <- newTVarIO [] :: IO (TVar [Int])
    runReaderT (test >> test >> pest) jabba
