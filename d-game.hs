module Babylon where

import Control.Concurrent
import Control.Concurrent.STM

import StartingInput
import Interface
import World

debWorld :: IO World
debWorld = do
    (o,s) <- debStartingInput
    makeNewWorld o s

