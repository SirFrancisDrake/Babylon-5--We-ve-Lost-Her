module Jumpgates where


import Navigation
import Vector

data Jumpgate = Jumpgate
    { jg_name :: String
    , jg_normal' :: Vector3D
    , jg_hyper' :: Vector3D
    }
    deriving (Eq)

instance Show Jumpgate where
  show (Jumpgate n v1 v2) = "Jumpgate ``" ++ n ++ "''\n\t" 
                            ++ show v1 ++ "\n\t" ++ show v2 ++ "\n"

jg_normal :: Jumpgate -> NavPosition
jg_normal j = Space (jg_normal' j) Normalspace

jg_hyper :: Jumpgate -> NavPosition
jg_hyper j = Space (jg_hyper' j) Hyperspace
