module JumpgatesData 
  ( jg_startingJumpgates
  )
where

import Jumpgates
import Navigation
import Vector

jg_startingJumpgates = zip [0..]
                           [ jg_io
                           ]

jg_io = Jumpgate "Io"
                 ( fromList [10,20,30] )
                 ( fromList [1,2,3] )
