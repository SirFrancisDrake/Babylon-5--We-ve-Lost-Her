
module Data.Encounters 
( encounters
)where

import Auxiliary.Zipper
import Data.Texts
import DataTypes
import Quests.Base

encounters =
  [ phone_booth
  ]

phone_booth :: Encounter
phone_booth = Encounter
  { encounter_check = checkQVarW (QSLocal $ q_title q_phone_booth) "finished" ((/=) (QBool True))
  , encounter_chance = 0.05
  , encounter_quest = q_phone_booth
  }

q_phone_booth =
  Quest 
    { q_title = t_q_phone_booth_title
    , q_screens = Zipper scr1 [ scr2, scr3, scr4, scr0]
    }
  where
  scr0 = Screen 0 undefined undefined
  scr1 =
    Screen 
      { s_id = 1
      , s_descr = t_q_phone_booth_s1_descr
      , s_actions =
        [ Action
          { a_descrs = [t_q_phone_booth_s1_a1_d1]
          , a_successCheck = return True
          , a_screenT = 2
          , a_modT = return ()
          , a_screenF = undefined
          , a_modF = return ()
          }
        ]
      }
  scr2 =
    Screen 
      { s_id = 2
      , s_descr = t_q_phone_booth_s2_descr 
      , s_actions =
        [ Action
          { a_descrs = [t_q_phone_booth_s2_a1_d1]
          , a_successCheck = return True
          , a_screenT = 3
          , a_modT = return ()
          , a_screenF = undefined
          , a_modF = return ()
          }
        , Action
          { a_descrs = [t_q_phone_booth_s2_a2_d1]
          , a_successCheck = return True
          , a_screenT = 4
          , a_modT = return ()
          , a_screenF = undefined
          , a_modF = return ()
          }
        ]
      }
  scr3 =
    Screen 
      { s_id = 3
      , s_descr = t_q_phone_booth_s3_descr
      , s_actions =
        [ Action
          { a_descrs = [t_q_phone_booth_s3_a1_d1]
          , a_successCheck = return True
          , a_screenT = 3
          , a_modT = return ()
          , a_screenF = undefined
          , a_modF = return ()
          }
        , Action
          { a_descrs = [t_q_phone_booth_s3_a2_d1]
          , a_successCheck = return True
          , a_screenT = 4
          , a_modT = return ()
          , a_screenF = undefined
          , a_modF = return ()
          }
        ]
      }
  scr4 =
    Screen 
      { s_id = 4
      , s_descr = t_q_phone_booth_s4_descr 
      , s_actions =
        [ Action
          { a_descrs = [t_q_phone_booth_s4_a1_d1]
          , a_successCheck = return True
          , a_screenT = 0
          , a_modT = addLocal "unfinished" (QBool False)
          , a_screenF = undefined
          , a_modF = undefined
          }
        ]
      }
