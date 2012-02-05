module Vector where
-- There's Data.Vector3D of course, but it's way too generic
-- It adds C-like vectors, while I need an instance of Num for 3D
-- So, it's quicker to write a dirty and slow implementation
-- Which this module is

import Data.Monoid

data Vector3D = Vector3D
    { vec_x :: Double
    , vec_y :: Double
    , vec_z :: Double
    } deriving (Eq, Show)

instance Num Vector3D where
    (Vector3D x1 y1 z1) + (Vector3D x2 y2 z2) = Vector3D (x1+x2) (y1+y2) (z1+z2)
    (-) a b = a + b * fromInteger (-1) -- for some very weird reason it's impossible to unadd Vec 0 0 0
    abs (Vector3D x y z) = undefined
    signum _ = undefined
    -- following 2 fns are there ONLY to write things like
    -- newPosition = oldPosition + velocity * numberOfTicks
    (*) (Vector3D x y z) (Vector3D x2 y2 z2) = Vector3D (x*x2) (y*y2) (z*z2)
    fromInteger i = let d = fromInteger i
                    in Vector3D d d d

length (Vector3D x y z) = sqrt( x^2 + y^2 + z^2 )

fromList (x:y:z:[]) = Vector3D x y z

instance Monoid Vector3D where
    mempty = Vector3D 0 0 0 -- not fromInteger 0 for the reasons stated above
    mappend = (+)
