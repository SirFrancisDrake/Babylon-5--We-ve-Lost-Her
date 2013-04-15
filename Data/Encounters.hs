
module Data.Encounters 
( encounters
)where

import Auxiliary.Zipper
import Encounters
import Quests.Q1

encounters =
  [ phone_booth
  ]

phone_booth :: Encounter
phone_booth = Encounter
  { encounter_check = return True
  , encounter_chance = 0.01
  , encounter_quest = q_phone_booth
  }

q_phone_booth =
  Quest 
    { q_title = "Phone booth"
    , q_screens = Zipper scr1 [ scr2, scr3, scr4]
    }
  where
  scr1 =
    Screen 
      { s_id = 1
      , s_descr = "As you let the autopilot drive, and go for a nap, you find yourself"++
                  " in a dream. You've been walking through a desert for weeks. " ++
                  "One day you see something unusual on the horizon. As you come closer"++
                  ", you find yourself looking at an old red phone booth. Most of the "++
                  "glass is broken, but other than that the boot is in astonishingly " ++
                  "good condition."
      , s_actions =
        [ Action
          { a_descrs = ["come close"]
          , a_check = const True
          , a_screenT = 2
          , a_screenF = undefined
          }
        ]
      }
  scr2 =
    Screen 
      { s_id = 2
      , s_descr = "You approach the booth. At close range it doesn't look that well-"++
                  "preserved. There are spots where the paint has come down, and you " ++
                  "can see rusty metal through the holes. The door seems to be jammed"
      , s_actions =
        [ Action
          { a_descrs = ["force the door"]
          , a_check = const True
          , a_screenT = 3
          , a_screenF = undefined
          }
        , Action
          { a_descrs = ["walk away"]
          , a_check = const True
          , a_screenT = 4
          , a_screenF = undefined
          }
        ]
      }
  scr3 =
    Screen 
      { s_id = 3
      , s_descr = "The door is jammed for good. You find yourself unable to open it."
      , s_actions =
        [ Action
          { a_descrs = ["try again"]
          , a_check = const True
          , a_screenT = 3
          , a_screenF = undefined
          }
        , Action
          { a_descrs = ["walk away"]
          , a_check = const True
          , a_screenT = 4
          , a_screenF = undefined
          }
        ]
      }
  scr4 =
    Screen 
      { s_id = 4
      , s_descr = "Sad and, maybe, even nostalgic, you slowly walk away into the desert."
      , s_actions =
        [ Action
          { a_descrs = ["end"]
          , a_check = const True
          , a_screenT = 0
          , a_screenF = undefined
          }
        ]
      }
