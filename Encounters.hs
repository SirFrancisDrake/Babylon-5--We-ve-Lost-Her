
module Encounters where

import Control.Concurrent.STM
import Control.Monad.Reader

import DataTypes
import Quests.Q1

data Encounter = Encounter
  { encounter_check :: ReaderT World STM Bool
  , encounter_chance :: Double
  , encounter_quest :: Quest
  }

