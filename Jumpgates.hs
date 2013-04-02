module Jumpgates where


import Navigation
import Vector

data Jumpgate = Jumpgate
    { jg_name :: String
    , jg_normalV :: Vector3D
    , jg_hyperV :: Vector3D
    }
    deriving (Eq)

instance Show Jumpgate where
  show (Jumpgate n v1 v2) = "Jumpgate ``" ++ n ++ "''\n\t" 
                            ++ show v1 ++ "\n\t" ++ show v2 ++ "\n"

jg_normal :: Jumpgate -> NavPosition
jg_normal j = Space (jg_normalV j) Normalspace

jg_hyper :: Jumpgate -> NavPosition
jg_hyper j = Space (jg_hyperV j) Hyperspace

jg_position :: Jumpgate -> SpaceType -> NavPosition
jg_position jg t =
  case t of
    Hyperspace  -> jg_normal jg
    Normalspace -> jg_hyper  jg

jg_vector :: Jumpgate -> SpaceType -> Vector3D
jg_vector jg t =
  case t of
    Hyperspace  -> jg_normalV jg
    Normalspace -> jg_hyperV  jg
