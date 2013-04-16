
module Interface.Base where

import Control.Concurrent.STM
import Control.Monad.Reader
import qualified Data.Map as M

data Menu_ActionAfter =
  MAA_Depends
  | MAA_Finish
  | MAA_Return
  deriving ()

data Menu_Result =
  MR_Pop
  | MR_Stay
  | MR_Top
  deriving ()
  
type Menu a = M.Map String (ReaderT a STM Bool, ReaderT a IO Menu_Result, Menu_ActionAfter)

