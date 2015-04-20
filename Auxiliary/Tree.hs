
module Auxiliary.Tree

import Data.Tree hiding (drawTree)
import qualified Data.Tree as T (drawTree)

-- As I've found out after writing it, it somewhat duplicates some of the functionality
-- of TravelBTree lib for Zipper monad.

data (Eq a) => ZTree a = ZTree -- Zipper for a Tree
  { zt_current :: Tree a
  , zt_up :: ZTree a
  , zt_left :: Forest a
  , zt_right :: Forest a
  } | ZEmpty
  deriving (Eq, Show)

zt_goDownTo :: (Eq a) => a -> ZTree a -> ZTree a
zt_goDownTo n t =
  let pr = \a -> rootLabel a == n
      sf = subForest $ zt_current t
      nf = filter pr sf
  in  if null nf
        then error "Auxiliary.Tree: Can't go down: matching label not found"
        else let (left, (center:right)) = break pr sf
                 ZTree c u l r = t
                 newUp = ZTree c{ subForest = [] } u l r
             in  ZTree center newUp left right 

zt_goUp :: (Eq a) => ZTree a -> ZTree a
zt_goUp (ZTree c u l r) =
  ZTree (zt_current u){ subForest = l ++ [c] ++ r } (zt_up u) (zt_left u) (zt_right u) 

foldToTree :: (Eq a) => ZTree a -> Tree a
foldToTree t =
  if zt_up t == ZEmpty
    then zt_current t
    else foldToTree (zt_goUp t)

draw :: (Show a, Eq a) => ZTree a -> String
draw = drawTree . foldToTree

drawTree :: (Show a) => Tree a -> String
drawTree = T.drawTree . (fmap show)
