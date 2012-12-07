module Sample where

import Q1

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
                                  , a_check = const True
                                  , a_screenT = 2
                                  , a_screenF = undefined
                                  }
                         , Action ["go east"]  (const True) 2 undefined
                         , Action ["go north"] (const True) 3 undefined
                         , Action ["climb tree"] (\g -> check g "dexterity" (>4)) 4 4
                         ]
           }

scr2 =
    Screen { s_id = 2
           , s_descr = "You see an old scary White house. There's a barred door which you seem to be unable to open."
           , s_actions = [ Action ["walk around"] (const True) 5 undefined
                         , Action ["break","bash"] (const True) 6 undefined
                         , Action ["knock"] (const True) 7 undefined
                         ]
           }
