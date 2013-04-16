module Quests.Sample where

import Auxiliary.Zipper
import Quests.Base
import Quests.Definitions

quest_sample =
    Quest { q_title = "Sample quest"
          , q_screens = Zipper scr1 [ scr2
                                    ]
         }

scr1 =
    Screen { s_id = 1
           , s_descr = "You are standing on a hill besides a tall and particularly beautiful white pine. " ++
                        "Westwards from you there is a large white house."
           , s_actions = [ Action { a_descrs = ["go west"]
                                  , a_successCheck = return True
                                  , a_screenT = 2
                                  , a_screenF = undefined
                                  }
                         , Action ["go east"]  (return True) 2 undefined
                         , Action ["go north"] (return True) 3 undefined
                         , Action ["climb tree"] (checkLocal "dexterity" (>4)) 4 4
                         ]
           }

scr2 =
    Screen { s_id = 2
           , s_descr = "You see an old scary White house. There's a barred door which you seem to be unable to open."
           , s_actions = [ Action ["walk around"] (return True) 5 undefined
                         , Action ["break","bash"] (return True) 6 undefined
                         , Action ["knock"] (return True) 7 undefined
                         ]
           }
